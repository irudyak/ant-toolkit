package com.anttoolkit.zookeeper.tasks;

import java.nio.charset.*;
import java.util.*;

import org.apache.tools.ant.*;
import org.apache.zookeeper.*;
import org.apache.zookeeper.data.*;

import com.anttoolkit.zookeeper.types.*;

public class CreateNodeTask extends GenericZookeeperTask
{
	private String node;
	private byte[] data;
	private CreateMode mode = CreateMode.PERSISTENT;
	private List<Permission> permissions = new LinkedList<Permission>();
	private List<ACL> acls = null;

	public void setNode(String node)
	{
		this.node = node;
	}

	public void setData(String data)
	{
		this.data = data == null ? null : data.getBytes(Charset.forName("UTF-8"));
	}

	public void setDataReference(String ref)
	{
		if (!this.checkReferenceExists(ref))
		{
			throw new BuildException("Specified reference '" + ref + "' doesn't exist");
		}

		Object obj = this.getReference(ref);
		if (obj == null || obj instanceof byte[])
		{
			data = (byte[])obj;
		}
		else
		{
			data = obj.toString().getBytes(Charset.forName("UTF-8"));
		}
	}

	public void setMode(String mode)
	{
		try
		{
			this.mode = CreateMode.valueOf(mode.trim().toUpperCase());
		}
		catch (IllegalArgumentException e)
		{
			throw new BuildException("Incorrect node creation mode specified: " + mode);
		}
	}

	public void addConfiguredPermission(Permission perm)
	{
		permissions.add(perm);
	}

	@Override
	public void doWork() throws BuildException
	{
		getZookeeperSession().create(node, data, getACLs(), mode);
	}

	@Override
	protected void validate()
	{
		if (node == null || node.trim().isEmpty())
		{
			throw new BuildException("Node name should be specified");
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
