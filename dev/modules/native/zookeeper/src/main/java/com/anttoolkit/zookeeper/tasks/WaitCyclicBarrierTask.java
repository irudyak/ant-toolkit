package com.anttoolkit.zookeeper.tasks;

import java.util.*;

import org.apache.tools.ant.*;
import org.apache.zookeeper.*;
import org.apache.zookeeper.data.*;

import com.anttoolkit.zookeeper.tasks.util.*;
import com.anttoolkit.general.tasks.concurrent.util.*;
import com.anttoolkit.general.common.*;

public class WaitCyclicBarrierTask
		extends GenericZookeeperTask
		implements Watcher
{
	private static final int READY_NODE_LIVE_TIMEOUT = 300000; //5 minutes

	private String name = null;
	private boolean echo = true;

	public void setName(String name)
	{
		this.name = name;
	}

	public void setEcho(boolean echo)
	{
		this.echo = echo;
	}

	@Override
	public void doWork() throws BuildException
	{
		CyclicBarrier barrier = SyncPrimitivesManager.getCyclicBarrier(name);
		if (barrier == null)
		{
			throw new BuildException("There are no cyclic barrier with name: " + name);
		}

		Thread thread = Thread.currentThread();
		TasksThread tasksThread = thread instanceof TasksThread ? (TasksThread)thread : null;

		if (echo)
		{
			if (tasksThread != null)
			{
				tasksThread.log("Waiting for barrier '" + name + "'");
			}
			else
			{
				this.log("Waiting for barrier '" + name + "'");
			}
		}

		await();

		if (echo)
		{
			if (tasksThread != null)
			{
				tasksThread.log("Waiting for barrier '" + name + "' completed");
			}
			else
			{
				this.log("Waiting for barrier '" + name + "' completed");
			}
		}
	}

	@Override
	public void process(WatchedEvent event)
	{
		synchronized (this)
		{
			this.notify();
		}
	}

	@Override
	protected void validate()
	{
		if (name == null || name.trim().isEmpty())
		{
			throw new BuildException("Cyclic barrier name should be specified");
		}
	}

	private void await()
	{
		CyclicBarrier barrier = SyncPrimitivesManager.getCyclicBarrier(name);

		String processNode = barrier.barrierNode + "/proc_";

		//registering a process
		try
		{
			processNode = getZookeeperSession().create(processNode, HOST_INFO, barrier.acls, CreateMode.EPHEMERAL_SEQUENTIAL);
		}
		catch (Throwable e)
		{
			throw new BuildException("Failed to create process node '" + processNode + "' for cyclic barrier '" + name + "'", e);
		}

		//sending notifications
		sendNotifications(barrier);

		//waiting for notification
		int index = processNode.lastIndexOf("/");
		String readyNode = barrier.readyNode + "/" + processNode.substring(index + 1);

		synchronized (this)
		{
			while (true)
			{
				if (getZookeeperSession().exists(readyNode, this))
				{
					break;
				}

				try
				{
					this.wait();
				}
				catch (InterruptedException e)
				{
					throw new BuildException("Failed to wait to perform for ready node '" + readyNode + "' of cyclic barrier '" + name + "'");
				}
			}
		}

		removeObsoleteNodes(barrier, processNode, readyNode);
	}

	private void sendNotifications(CyclicBarrier barrier)
	{
		List<String> processes;

		try
		{
			processes = getZookeeperSession().getChildren(barrier.barrierNode, false);
		}
		catch (Throwable e)
		{
			throw new BuildException("Failed to get cyclic barrier '" + name + "' process list from node '" + barrier.barrierNode + "'", e);
		}

		processes.remove(barrier.readyNode);

		if (processes.size() < barrier.parties)
		{
			return;
		}

		List<String> notifications;

		try
		{
			notifications = getZookeeperSession().getChildren(barrier.readyNode, false);
		}
		catch (Throwable e)
		{
			throw new BuildException("Failed to get cyclic barrier '" + name + "' notifications list from node '" + barrier.readyNode + "'", e);
		}

		removeNodeFullPaths(processes);
		removeNodeFullPaths(notifications);
		processes.removeAll(notifications);

		boolean rerunNotifications = false;

		while (processes.size() >= barrier.parties)
		{
			Collections.sort(processes);
			notifications = new LinkedList<String>();

			for (int i = 0; i < barrier.parties; i++)
			{
				notifications.add(processes.get(i));
			}

			if (!createNotifications(barrier, notifications))
			{
				rerunNotifications = true;
				rerunNotifications = true;
				break;
			}

			processes.removeAll(notifications);
		}

		if (rerunNotifications)
		{
			sendNotifications(barrier);
		}
	}

	private void removeNodeFullPaths(List<String> nodes)
	{
		for (int i = 0; i < nodes.size(); i++)
		{
			String node = nodes.get(i);
			int index = node.lastIndexOf("/");
			node = node.substring(index + 1);
			nodes.set(i, node);
		}
	}

	private boolean createNotifications(CyclicBarrier barrier, List<String> notifications)
	{
		getZookeeperSession().startTransaction();

		for (String notification : notifications)
		{
			getZookeeperSession().create(barrier.readyNode + "/" + notification, HOST_INFO, barrier.acls, CreateMode.PERSISTENT);
		}

		try
		{
			getZookeeperSession().commitTransaction();
		}
		catch (Throwable e)
		{
			if (ExceptionHelper.hasCauseInChain(KeeperException.NodeExistsException.class, e))
			{
				return false;
			}

			StringBuilder builder = new StringBuilder();
			for (String notification : notifications)
			{
				if (builder.length() > 0)
				{
					builder.append(", ");
				}

				builder.append(notification);
			}

			throw new BuildException("Failed to create notifications '" + notifications.toString() + "' inside node '" + barrier.readyNode + "'", e);
		}

		return true;
	}

	private long getNodeSequenceNumber(String node)
	{
		int index = node.lastIndexOf("_");

		try
		{
			return Long.parseLong(node.substring(index + 1));
		}
		catch (NumberFormatException e)
		{
			throw new BuildException("Incorrect sequential node name: " + node, e);
		}
	}

	private void removeObsoleteNodes(CyclicBarrier barrier, String processNode, String readyNode)
	{
		//removing process node
		try
		{
			getZookeeperSession().delete(processNode, false, -1);
		}
		catch (Throwable e)
		{
			if (!ExceptionHelper.hasCauseInChain(KeeperException.NoNodeException.class, e))
			{
				this.log("Failed to delete process node '" + processNode + "' for cyclic barrier '" + name + "'", e, Project.MSG_WARN);
			}
		}

		//getting info about ready node
		Stat stat = new Stat();

		try
		{
			getZookeeperSession().getData(readyNode, stat);
		}
		catch (Throwable e)
		{
			if (!ExceptionHelper.hasCauseInChain(KeeperException.NoNodeException.class, e))
			{
				this.log("Failed to get information about ready node '" + processNode + "' for cyclic barrier '" + name + "'", e, Project.MSG_WARN);
			}
			stat = null;
		}

		//removing ready node
		try
		{
			getZookeeperSession().delete(readyNode, false, -1);
		}
		catch (Throwable e)
		{
			if (!ExceptionHelper.hasCauseInChain(KeeperException.NoNodeException.class, e))
			{
				this.log("Failed to delete ready node '" + processNode + "' for cyclic barrier '" + name + "'", e, Project.MSG_WARN);
			}
		}

		if (stat != null)
		{
			removeObsoleteReadyNodes(barrier, getNodeSequenceNumber(readyNode), stat.getCtime());
		}
	}

	private void removeObsoleteReadyNodes(CyclicBarrier barrier, long maxNodeNumber, long maxCreationTime)
	{
		List<String> nodes = null;

		try
		{
			nodes = getZookeeperSession().getChildren(barrier.readyNode, false);
		}
		catch (Throwable e)
		{
			this.log("Failed to get list of ready nodes for cyclic barrier '" + name + "'", e, Project.MSG_WARN);
			return;
		}

		Stat stat = new Stat();

		for (String node : nodes)
		{
			if (getNodeSequenceNumber(node) > maxNodeNumber)
			{
				continue;
			}

			try
			{
				getZookeeperSession().getData(node, stat);
				if (stat.getCtime() - maxCreationTime >= READY_NODE_LIVE_TIMEOUT)
				{
					getZookeeperSession().delete(node, false, -1);
				}
			}
			catch (Throwable e)
			{
			}
		}
	}
}
