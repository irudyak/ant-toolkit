package com.anttoolkit.zookeeper.tasks;

import java.nio.charset.*;
import java.util.*;

import org.apache.tools.ant.*;
import org.apache.zookeeper.*;
import org.apache.zookeeper.data.*;

import com.anttoolkit.zookeeper.types.*;
import com.anttoolkit.zookeeper.tasks.util.*;
import com.anttoolkit.general.common.*;

public class CreateCyclicBarrierTask
		extends GenericZookeeperTask
{
	private String root;
	private String name;
	private int parties = -1;
	private List<Permission> permissions = new LinkedList<Permission>();
	private List<ACL> acls;

	public void setName(String name)
	{
		this.name = name;
	}

	public void setParties(int parties)
	{
		this.parties = parties;
	}

	public void setRootNode(String root)
	{
		this.root = root;
	}

	public void addConfiguredPermission(Permission perm)
	{
		permissions.add(perm);
	}

	public void doWork() throws BuildException
	{
		CyclicBarrier barrier = new CyclicBarrier(root, name, parties, getACLs());

		//try to create base node for cyclic barrier
		try
		{
			this.getZookeeperSession().create(barrier.barrierNode, Integer.toString(parties) + "/" + HOST_INFO_STR, getACLs(), CreateMode.PERSISTENT);
		}
		catch (BuildException e)
		{
			if (!(e.getCause() instanceof KeeperException.NodeExistsException))
			{
				throw new BuildException("Failed to create node '" + barrier.barrierNode + "' for cyclic barrier '" + name + "'", e);
			}
		}

		//try to create 'ready' node for cyclic barrier
		try
		{
			this.getZookeeperSession().create(barrier.readyNode, Integer.toString(parties) + "/" + HOST_INFO_STR, getACLs(), CreateMode.PERSISTENT);
		}
		catch (BuildException e)
		{
			if (!(e.getCause() instanceof KeeperException.NodeExistsException))
			{
				throw new BuildException("Failed to create node '" + barrier.readyNode + "' for cyclic barrier '" + name + "'", e);
			}
		}

		SyncPrimitivesManager.addCyclicBarrier(name, barrier);
	}

	protected void validate()
	{
		if (root == null || root.trim().isEmpty())
		{
			throw new BuildException("Barrier root node should be specified");
		}

		if (name == null || name.trim().isEmpty())
		{
			throw new BuildException("Barrier name should be specified");
		}

		if (parties <= 0)
		{
			throw new BuildException("Parties value should be specified greater than zero");
		}
	}

	private List<ACL> getACLs()
	{
		if (acls != null)
		{
			return acls;
		}

		if (permissions.isEmpty())
		{
			return acls = ZooDefs.Ids.OPEN_ACL_UNSAFE;
		}

		acls = new LinkedList<ACL>();

		for (Permission perm : permissions)
		{
			acls.add(perm.getACL());
		}

		return acls;
	}
}
