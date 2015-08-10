package com.anttoolkit.zookeeper.tasks;

import java.util.*;

import com.anttoolkit.general.common.SystemHelper;
import org.apache.tools.ant.*;
import org.apache.zookeeper.*;
import org.apache.zookeeper.data.*;

public class GetNodeACLTask extends GenericZookeeperTask
{
	private String node;
	private String property;
	private String reference;

	public void setNode(String node)
	{
		this.node = node;
	}

	public void setProperty(String property)
	{
		this.property = property;
	}

	public void setReference(String reference)
	{
		this.reference = reference;
	}

	@Override
	public void doWork() throws BuildException
	{
		List<ACL> acls = getZookeeperSession().getACL(node);

		if (reference != null)
		{
			this.setReference(reference, acls);
		}

		if (property != null)
		{
			this.setPropertyThreadSafe(property, convertToString(acls));
		}
	}

	@Override
	protected void validate()
	{
		if (node == null || node.trim().isEmpty())
		{
			throw new BuildException("Node name should be specified");
		}

		if (property == null && reference == null)
		{
			throw new BuildException("Property or reference should be specified");
		}

		if (!getZookeeperSession().exists(node))
		{
			throw new BuildException("Specified ZooKeeper node doesn't exist: " + node);
		}
	}

	private String convertToString(List<ACL> acls)
	{
		if (acls == null || acls.isEmpty())
		{
			return "";
		}

		StringBuilder builder = new StringBuilder();

		for (ACL acl : acls)
		{
			if (builder.length() != 0)
			{
				builder.append(SystemHelper.lineSeparator);
			}

			builder.append(acl.getId().getScheme() + ", " + acl.getId().getId() + ", " + getPermString(acl.getPerms()));
		}

		return builder.toString();
	}

	private String getPermString(int perms)
	{
		StringBuilder p = new StringBuilder();

		if ((perms & ZooDefs.Perms.CREATE) != 0)
		{
			p.append('c');
		}

		if ((perms & ZooDefs.Perms.DELETE) != 0)
		{
			p.append('d');
		}

		if ((perms & ZooDefs.Perms.READ) != 0)
		{
			p.append('r');
		}

		if ((perms & ZooDefs.Perms.WRITE) != 0)
		{
			p.append('w');
		}

		if ((perms & ZooDefs.Perms.ADMIN) != 0)
		{
			p.append('a');
		}

		return p.toString();
	}
}
