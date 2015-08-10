package com.anttoolkit.zookeeper.common;

import java.nio.charset.*;
import java.util.*;

import org.apache.tools.ant.*;
import org.apache.zookeeper.*;
import org.apache.zookeeper.Watcher.Event.*;
import org.apache.zookeeper.data.*;

import com.anttoolkit.zookeeper.types.*;

public class ZookeeperSession implements Watcher
{
	private static final int CONNECTION_WAIT_TIMEOUT = 100;	//milliseconds
	private static final int OPERATION_TIMEOUT = 300;
	private static final int OPERATION_ATTEMPTS = 50;

	private ZookeeperConfig conf;
	private ZooKeeper zookeeper;
	private volatile Event.KeeperState state = KeeperState.Disconnected;
	private List<Op> transaction;

	ZookeeperSession(ZookeeperConfig conf)
	{
		this.conf = conf;
	}

	public String create(String path, String data, List<ACL> acl, CreateMode createMode)
	{
		return create(path, data == null ? null : data.getBytes(Charset.forName("UTF-8")), acl, createMode);
	}

	public String create(String path, byte[] data, List<ACL> acl, CreateMode createMode)
	{
		String[] parts = path.split("/", -1);

		StringBuilder currentNode = new StringBuilder();

		String createdNode = null;

		for (String node : parts)
		{
			if (node.trim().isEmpty())
			{
				continue;
			}

			currentNode.append("/").append(node);

			if (nodeExists(currentNode.toString(), null))
			{
				continue;
			}

			if (currentNode.toString().equals(path))
			{
				createdNode = createNode(currentNode.toString(), data, acl, createMode);
			}
			else
			{
				if (createMode.equals(CreateMode.EPHEMERAL) ||
					createMode.equals(CreateMode.EPHEMERAL_SEQUENTIAL))
				{
					throw new BuildException("It is impossible to create a hierarchy of EPHEMERAL nodes: " + node);
				}

				createNode(currentNode.toString(), null, acl, createMode);
			}
		}

		return createdNode;
	}

	public void delete(String path, boolean recursive, int version)
	{
		if (!recursive)
		{
			deleteNode(path, version);
			return;
		}

		List<String> children = getChildren(path, false);
		for (String child : children)
		{
			delete(child, true, -1);
		}

		deleteNode(path, -1);
	}

	public boolean exists(String path)
	{
		return nodeExists(path, null);
	}

	public boolean exists(String path, Watcher watcher)
	{
		return nodeExists(path, watcher);
	}

	public byte[] getData(String path, Stat stat)
	{
		return getNodeData(path, stat);
	}

	public void setData(String path, int version, byte[] data)
	{
		setNodeData(path, version, data);
	}

	public List<String> getChildren(String path, boolean recursive)
	{
		if (!this.nodeExists(path, null))
		{
			throw new BuildException("Specified node '" + path + "' doesn't exist");
		}

		List<String> children = new LinkedList<String>();

		getNodeChildren(path, children, recursive);

		Collections.sort(children);

		return children;
	}

	public void setACL(String path, int version, List<ACL> acl)
	{
		setNodeAcl(path, version, acl);
	}

	public List<ACL> getACL(String path)
	{
		return getNodeAcl(path);
	}

	public void check(String path, int version)
	{
		if (transaction == null)
		{
			throw new BuildException("Check operation only supported inside ZooKeeper transaction");
		}

		transaction.add(Op.check(path, version));
	}

	public void close()
	{
		if (zookeeper == null)
		{
			return;
		}

		try
		{
			zookeeper.close();
		}
		catch (Throwable e)
		{
		}
		finally
		{
			zookeeper = null;
			state = KeeperState.Disconnected;
		}
	}

	public void startTransaction()
	{
		if (transaction != null)
		{
			throw new BuildException("Nested transactions are not supported");
		}

		transaction = new LinkedList<Op>();
	}

	public void commitTransaction()
	{
		if (transaction == null)
		{
			throw new BuildException("There are no opened transaction to commit");
		}

		Throwable ex = null;

		try
		{
			for (int i = 0; i < OPERATION_ATTEMPTS; i++)
			{
				try
				{
					getZooKeeper().multi(transaction);
					return;
				}
				catch (KeeperException e)
				{
					if (!(e instanceof KeeperException.ConnectionLossException) &&
						!(e instanceof KeeperException.SessionExpiredException))
					{
						throw new BuildException("Failed to commit ZooKeeper transaction", e);
					}

					ex = e;

					try
					{
						Thread.sleep(OPERATION_TIMEOUT);
					}
					catch (InterruptedException exc)
					{
						throw new BuildException("Failed to commit ZooKeeper transaction", exc);
					}
				}
				catch (InterruptedException e)
				{
					throw new BuildException("Failed to commit ZooKeeper transaction", e);
				}
			}
		}
		finally
		{
			transaction = null;
		}

		throw new BuildException("Failed to commit ZooKeeper transaction", ex);
	}

	@Override
	public void process(WatchedEvent event)
	{
		state = event.getState();
		//System.out.println("STATE=" + event.getState());
	}

	private ZooKeeper getZooKeeper()
	{
		if (zookeeper != null && KeeperState.AuthFailed.equals(getState()))
		{
			throw new BuildException("Failed to authenticate to ZooKeeper ");
		}

		if (zookeeper != null && KeeperState.Expired.equals(getState()))
		{
			this.close();
		}

		if (zookeeper == null)
		{
			if (conf.getConnectionString() == null || conf.getConnectionString().trim().isEmpty())
			{
				throw new BuildException("Connection string should be specified for ZooKeeper session");
			}

			if (conf.getTimeout() <= 0)
			{
				throw new BuildException("ZooKeeper session timeout should be specified");
			}

			if (conf.getTimeout() <= 1)
			{
				throw new BuildException("Incorrect ZooKeeper session timeout specified: " + conf.getTimeout());
			}

			try
			{
				this.zookeeper = new ZooKeeper(conf.getConnectionString(), conf.getTimeout(), this);
				for (AuthInfo info : conf.getAuthInfo())
				{
					this.zookeeper.addAuthInfo(info.getScheme(), info.getInfo());
				}

				// wait some minimum time so that AuthInfo will be applied to created ZooKeeper instance
				Thread.sleep(CONNECTION_WAIT_TIMEOUT);
			}
			catch (Throwable e)
			{
				throw new BuildException("Failed to create new ZooKeeper object", e);
			}
		}

		if (!getState().equals(KeeperState.SyncConnected))
		{
			waitForConnectionToEstablish();
		}

		return zookeeper;
	}

	private void setState(Event.KeeperState state)
	{
		this.state = state;
	}

	private Event.KeeperState getState()
	{
		return state;
	}

	private void waitForConnectionToEstablish()
	{
		int waitTime = 0;

		while (waitTime <= conf.getTimeout())
		{
			if (getState().equals(KeeperState.SyncConnected))
			{
				return;
			}

			try
			{
				Thread.sleep(CONNECTION_WAIT_TIMEOUT);
			}
			catch (InterruptedException e)
			{
				throw new BuildException("Waiting of session establishment was interrupted");
			}

			waitTime += CONNECTION_WAIT_TIMEOUT;
		}

		throw new BuildException("Failed to connect to ZooKeeper ensemble during specified session timeout: " + conf.getTimeout() + " milliseconds");
	}

	private boolean nodeExists(String node, Watcher watcher)
	{
		Throwable ex = null;

		for (int i = 0; i < OPERATION_ATTEMPTS; i++)
		{
			try
			{
				if (watcher == null)
				{
					return getZooKeeper().exists(node, false) != null;
				}
				else
				{
					return getZooKeeper().exists(node, watcher) != null;
				}
			}
			catch (KeeperException e)
			{
				if (!(e instanceof KeeperException.ConnectionLossException) &&
					!(e instanceof KeeperException.SessionExpiredException))
				{
					throw new BuildException("Failed to check if ZooKeeper node '" + node + "' exists", e);
				}

				ex = e;

				try
				{
					Thread.sleep(OPERATION_TIMEOUT);
				}
				catch (InterruptedException exc)
				{
					throw new BuildException("Failed to check if ZooKeeper node '" + node + "' exists", exc);
				}
			}
			catch (InterruptedException e)
			{
				throw new BuildException("Failed to check if ZooKeeper node '" + node + "' exists", e);
			}
		}

		throw new BuildException("Failed to check if ZooKeeper node '" + node + "' exists", ex);
	}

	private String createNode(String path, byte[] data, List<ACL> acl, CreateMode createMode)
	{
		Throwable ex = null;

		for (int i = 0; i < OPERATION_ATTEMPTS; i++)
		{
			try
			{
				if (transaction != null)
				{
					transaction.add(Op.create(path, data == null ? new byte[0] : data, acl, createMode));
					return null;
				}
				else
				{
					return getZooKeeper().create(path, data, acl, createMode);
				}
			}
			catch (KeeperException e)
			{
				if (!(e instanceof KeeperException.ConnectionLossException) &&
					!(e instanceof KeeperException.SessionExpiredException))
				{
					throw new BuildException("Failed to create ZooKeeper node: " + path, e);
				}

				ex = e;

				try
				{
					Thread.sleep(OPERATION_TIMEOUT);
				}
				catch (InterruptedException exc)
				{
					throw new BuildException("Failed to create ZooKeeper node: " + path, exc);
				}
			}
			catch (InterruptedException e)
			{
				throw new BuildException("Failed to create ZooKeeper node: " + path, e);
			}
		}

		throw new BuildException("Failed to create ZooKeeper node: " + path, ex);
	}

	private void deleteNode(String path, int version)
	{
		Throwable ex = null;

		for (int i = 0; i < OPERATION_ATTEMPTS; i++)
		{
			try
			{
				if (transaction != null)
				{
					transaction.add(Op.delete(path, version));
				}
				else
				{
					getZooKeeper().delete(path, version);
				}

				return;
			}
			catch (KeeperException e)
			{
				if (!(e instanceof KeeperException.ConnectionLossException) &&
					!(e instanceof KeeperException.SessionExpiredException))
				{
					throw new BuildException("Failed to delete ZooKeeper node: " + path + ", " + version, e);
				}

				ex = e;

				try
				{
					Thread.sleep(OPERATION_TIMEOUT);
				}
				catch (InterruptedException exc)
				{
					throw new BuildException("Failed to delete ZooKeeper node: " + path + ", " + version, exc);
				}
			}
			catch (InterruptedException e)
			{
				throw new BuildException("Failed to delete ZooKeeper node: " + path + ", " + version, e);
			}
		}

		throw new BuildException("Failed to delete ZooKeeper node: " + path + ", " + version, ex);
	}

	private byte[] getNodeData(String path, Stat stat)
	{
		Throwable ex = null;

		for (int i = 0; i < OPERATION_ATTEMPTS; i++)
		{
			try
			{
				return getZooKeeper().getData(path, false, stat);
			}
			catch (KeeperException e)
			{
				if (!(e instanceof KeeperException.ConnectionLossException) &&
					!(e instanceof KeeperException.SessionExpiredException))
				{
					throw new BuildException("Failed to get ZooKeeper node data: " + path, e);
				}

				ex = e;

				try
				{
					Thread.sleep(OPERATION_TIMEOUT);
				}
				catch (InterruptedException exc)
				{
					throw new BuildException("Failed to get ZooKeeper node data: " + path, exc);
				}
			}
			catch (InterruptedException e)
			{
				throw new BuildException("Failed to get ZooKeeper node data: " + path, e);
			}
		}

		throw new BuildException("Failed to get ZooKeeper node data: " + path, ex);
	}

	private void setNodeData(String path, int version, byte[] data)
	{
		Throwable ex = null;

		for (int i = 0; i < OPERATION_ATTEMPTS; i++)
		{
			try
			{
				if (transaction != null)
				{
					transaction.add(Op.setData(path, data == null ? new byte[0] : data, version));
				}
				else
				{
					getZooKeeper().setData(path, data == null ? new byte[0] : data, version);
				}

				return;
			}
			catch (KeeperException e)
			{
				if (!(e instanceof KeeperException.ConnectionLossException) &&
					!(e instanceof KeeperException.SessionExpiredException))
				{
					throw new BuildException("Failed to set data for ZooKeeper node: " + path, e);
				}

				ex = e;

				try
				{
					Thread.sleep(OPERATION_TIMEOUT);
				}
				catch (InterruptedException exc)
				{
					throw new BuildException("Failed to set data for ZooKeeper node: " + path, exc);
				}
			}
			catch (InterruptedException e)
			{
				throw new BuildException("Failed to set data for ZooKeeper node: " + path, e);
			}
		}

		throw new BuildException("Failed to set data for ZooKeeper node: " + path, ex);
	}

	private void getNodeChildren(String path, List<String> children, boolean recursive)
	{
		Throwable ex = null;
		List<String> nodes = null;

		for (int i = 0; i < OPERATION_ATTEMPTS; i++)
		{
			try
			{
				nodes = getZooKeeper().getChildren(path, false);
				ex = null;
				break;
			}
			catch (KeeperException e)
			{
				if (!(e instanceof KeeperException.ConnectionLossException) &&
					!(e instanceof KeeperException.SessionExpiredException))
				{
					throw new BuildException("Failed to get children of ZooKeeper node: " + path, e);
				}

				ex = e;

				try
				{
					Thread.sleep(OPERATION_TIMEOUT);
				}
				catch (InterruptedException exc)
				{
					throw new BuildException("Failed to get children of ZooKeeper node: " + path, exc);
				}
			}
			catch (InterruptedException e)
			{
				throw new BuildException("Failed to get children of ZooKeeper node: " + path, e);
			}
		}

		if (ex != null)
		{
			throw new BuildException("Failed to get children of ZooKeeper node: " + path, ex);
		}

		if (nodes == null || nodes.isEmpty())
		{
			return;
		}

		for (String node : nodes)
		{
			String child = path.endsWith("/") ? path + node : path + "/" + node;

			children.add(child);

			if (recursive)
			{
				getNodeChildren(child, children, true);
			}
		}
	}

	private void setNodeAcl(String path, int version, List<ACL> acl)
	{
		Throwable ex = null;

		for (int i = 0; i < OPERATION_ATTEMPTS; i++)
		{
			try
			{
				getZooKeeper().setACL(path, acl, version);
				return;
			}
			catch (KeeperException e)
			{
				if (!(e instanceof KeeperException.ConnectionLossException) &&
					!(e instanceof KeeperException.SessionExpiredException))
				{
					throw new BuildException("Failed to set ACL for ZooKeeper node: " + path + ", " + version, e);
				}

				ex = e;

				try
				{
					Thread.sleep(OPERATION_TIMEOUT);
				}
				catch (InterruptedException exc)
				{
					throw new BuildException("Failed to set ACL for ZooKeeper node: " + path + ", " + version, exc);
				}
			}
			catch (InterruptedException e)
			{
				throw new BuildException("Failed to set ACL for ZooKeeper node: " + path + ", " + version, e);
			}
		}

		throw new BuildException("Failed to set ACL for ZooKeeper node: " + path + ", " + version, ex);
	}

	private List<ACL> getNodeAcl(String path)
	{
		Throwable ex = null;

		for (int i = 0; i < OPERATION_ATTEMPTS; i++)
		{
			try
			{
				Stat stat = new Stat();
				return getZooKeeper().getACL(path, stat);
			}
			catch (KeeperException e)
			{
				if (!(e instanceof KeeperException.ConnectionLossException) &&
					!(e instanceof KeeperException.SessionExpiredException))
				{
					throw new BuildException("Failed to get ACL of ZooKeeper node: " + path, e);
				}

				ex = e;

				try
				{
					Thread.sleep(OPERATION_TIMEOUT);
				}
				catch (InterruptedException exc)
				{
					throw new BuildException("Failed to get ACL of ZooKeeper node: " + path, exc);
				}
			}
			catch (InterruptedException e)
			{
				throw new BuildException("Failed to get ACL of ZooKeeper node: " + path, e);
			}
		}

		throw new BuildException("Failed to get ACL of ZooKeeper node: " + path, ex);
	}
}
