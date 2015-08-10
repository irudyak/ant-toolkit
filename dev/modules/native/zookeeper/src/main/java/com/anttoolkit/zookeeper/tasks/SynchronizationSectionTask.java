package com.anttoolkit.zookeeper.tasks;

import java.nio.charset.*;
import java.util.*;

import org.apache.tools.ant.*;
import org.apache.zookeeper.*;
import org.apache.zookeeper.data.*;

import com.anttoolkit.zookeeper.types.*;
import com.anttoolkit.general.common.*;
import com.anttoolkit.general.tasks.concurrent.util.*;

public class SynchronizationSectionTask
		extends GenericZookeeperTask
		implements TaskContainer, Watcher
{
	private enum LockType
	{
		READ, WRITE
	}

	private class LockInfo
	{
		public final boolean isReadLock;
		public final int lockNumber;

		public LockInfo(LockType lockType, int lockNumber)
		{
			this.isReadLock = LockType.READ.equals(lockType);
			this.lockNumber = lockNumber;
		}
	}

	private String root;
	private String sectionName;
	private List<ACL> acls = ZooDefs.Ids.OPEN_ACL_UNSAFE;
	private LockType lockType = LockType.WRITE;
	private boolean echo = true;
	private List<Task> tasks = new LinkedList<Task>();

	public void setRootNode(String root)
	{
		this.root = root;
	}

	public void setSectionName(String name)
	{
		this.sectionName = name;
	}

	public void setEcho(boolean echo)
	{
		this.echo = echo;
	}

	public void setAclReference(String aclRef)
	{
		if (!this.checkReferenceExists(aclRef))
		{
			throw new BuildException("Specified ACL reference '" + aclRef + "' doesn't exist");
		}

		Object obj = this.getReference(aclRef);
		if (!(obj instanceof ACLS))
		{
			throw new BuildException("Specified object reference '" + aclRef + "' doesn't contain ACL object");
		}

		acls = ((ACLS)obj).getACLs();
	}

	public void setLockType(String lockType)
	{
		try
		{
			this.lockType = LockType.valueOf(lockType);
		}
		catch (IllegalArgumentException e)
		{
			throw new BuildException("Invalid lock type specified: " + lockType);
		}
	}

	@Override
	public void doWork() throws BuildException
	{
		String baseNode = root.endsWith("/") ? root + sectionName : root + "/" + sectionName;

		//try to create base node for all locks, if it's not already created
		try
		{
			getZookeeperSession().create(baseNode, HOST_INFO, acls, CreateMode.PERSISTENT);
		}
		catch (BuildException e)
		{
			if (!(e.getCause() instanceof KeeperException.NodeExistsException))
			{
				throw new BuildException("Failed to create base node for critical section '" + sectionName + "'", e);
			}
		}

		Thread thread = Thread.currentThread();
		TasksThread tasksThread = thread instanceof TasksThread ? (TasksThread)thread : null;

		if (echo)
		{
			if (tasksThread != null)
			{
				tasksThread.log("Trying to enter synchronization section '" + sectionName + "' using " + lockType + " lock");
			}
			else
			{
				this.log("Trying to enter synchronization section '" + sectionName + "' using " + lockType + " lock");
			}
		}

		//perform lock
		String lockNode = lock(baseNode);

		if (echo)
		{
			if (tasksThread != null)
			{
				tasksThread.log("Entered synchronization section '" + sectionName + "' using " + lockType + " lock");
			}
			else
			{
				this.log("Entered synchronization section '" + sectionName + "' using " + lockType + " lock");
			}
		}

		try
		{
			//execute tasks
			for (Task task : tasks)
			{
				task.perform();
			}
		}
		finally
		{
			if (echo)
			{
				if (tasksThread != null)
				{
					tasksThread.log("Left synchronization section '" + sectionName + "' having " + lockType + " lock");
				}
				else
				{
					this.log("Left synchronization section '" + sectionName + "' having " + lockType + " lock");
				}
			}

			//remove lock
			unlock(lockNode);

			//try to remove base node, if it doesn't contain any children
			try
			{
				getZookeeperSession().delete(baseNode, false, -1);
			}
			catch (BuildException e)
			{
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
	public void addTask(Task task)
	{
		tasks.add(task);
	}

	@Override
	protected void validate()
	{
		if (root == null || root.trim().isEmpty())
		{
			throw new BuildException("Root node should be specified");
		}

		if (sectionName == null || sectionName.trim().isEmpty())
		{
			throw new BuildException("Synchronization section name should be specified");
		}

		if (!getZookeeperSession().exists(root))
		{
			throw new BuildException("Specified ZooKeeper root node '" + root + "' doesn't exist");
		}
	}

	private String lock(String baseNode)
	{
		String lockNode = baseNode + "/" + lockType.toString() + "_";

		try
		{
			lockNode = getZookeeperSession().create(lockNode, HOST_INFO, acls, CreateMode.EPHEMERAL_SEQUENTIAL);
		}
		catch (BuildException e)
		{
			throw new BuildException("Failed to create '" + lockNode + "' node to perform " + lockType +
					" lock for synchronization section '" + sectionName + "'");
		}

		synchronized (this)
		{
			while (true)
			{
				String nodeToWatch = getPreviousLockNodeToWatch(baseNode, lockNode);
				if (nodeToWatch == null)
				{
					break;
				}

				if (getZookeeperSession().exists(nodeToWatch, this))
				{
					try
					{
						this.wait();
					}
					catch (InterruptedException e)
					{
						throw new BuildException("Failed to wait to perform '" + lockNode + "' lock");
					}
				}
			}
		}

		return lockNode;
	}

	private void unlock(String lockNode)
	{
		try
		{
			getZookeeperSession().delete(lockNode, false, -1);
		}
		catch (BuildException e)
		{
			log("Failed to remove " + lockType + " lock node: " + lockNode, e, Project.MSG_WARN);
		}
	}

	private String getPreviousLockNodeToWatch(String baseNode, String lockNode)
	{
		List<String> children = getZookeeperSession().getChildren(baseNode, false);
		LockInfo currentLockInfo = getLockInfo(lockNode);
		int lockNodeNumberToWatch = -1;
		String lockNodeToWatch = null;

		for (String node : children)
		{
			LockInfo lockInfo = getLockInfo(node);

			if (currentLockInfo.isReadLock)
			{
				if (!lockInfo.isReadLock &&
					lockInfo.lockNumber < currentLockInfo.lockNumber &&
					lockInfo.lockNumber > lockNodeNumberToWatch)
				{
					lockNodeNumberToWatch = lockInfo.lockNumber;
					lockNodeToWatch = node;
				}
			}
			else
			{
				if (lockInfo.lockNumber < currentLockInfo.lockNumber &&
					lockInfo.lockNumber > lockNodeNumberToWatch)
				{
					lockNodeNumberToWatch = lockInfo.lockNumber;
					lockNodeToWatch = node;
				}
			}
		}

		return lockNodeToWatch;
	}

	private LockInfo getLockInfo(String node)
	{
		String[] parts = node.split("/", -1);
		parts = parts[parts.length - 1].split("_", -1);

		if (parts.length != 2)
		{
			throw new IllegalStateException("Incorrect lock node name: " + node);
		}

		int lockNumber = -1;
		LockType lockType = null;

		try
		{
			lockNumber = Integer.parseInt(parts[1]);
			lockType = LockType.valueOf(parts[0]);
		}
		catch (NumberFormatException e)
		{
			throw new IllegalStateException("Incorrect lock node name: " + node);
		}
		catch (IllegalArgumentException e)
		{
			throw new IllegalStateException("Incorrect lock node name: " + node);
		}

		if (lockNumber < 0)
		{
			throw new IllegalStateException("Incorrect lock node name: " + node);
		}

		return new LockInfo(lockType, lockNumber);
	}
}
