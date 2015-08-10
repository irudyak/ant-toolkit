package com.anttoolkit.zookeeper.tasks;

import java.util.*;

import org.apache.tools.ant.*;
import org.apache.zookeeper.data.*;

import com.anttoolkit.zookeeper.types.*;

public class SetNodeACLTask extends GenericZookeeperTask
{
	private String node;
	private int version = -1;
	private List<Permission> permissions = new LinkedList<Permission>();

	public void setNode(String node)
	{
		this.node = node;
	}

	public void setVersion(int version)
	{
		this.version = version;
	}

	public void addConfiguredPermission(Permission perm)
	{
		permissions.add(perm);
	}

	@Override
	public void doWork() throws BuildException
	{
		getZookeeperSession().setACL(node, version, getACLs());
	}

	@Override
	protected void validate()
	{
		if (node == null || node.trim().isEmpty())
		{
			throw new BuildException("Node name should be specified");
		}

		if (permissions.isEmpty())
		{
			throw new BuildException("There are no permissions specified for ZooKeeper node: " + node);
		}

		if (!getZookeeperSession().exists(node))
		{
			throw new BuildException("Specified ZooKeeper node doesn't exist: " + node);
		}
	}

	private List<ACL> getACLs()
	{
		List<ACL> acls = new LinkedList<ACL>();

		for (Permission perm : permissions)
		{
			acls.add(perm.getACL());
		}

		return acls;
	}
}
