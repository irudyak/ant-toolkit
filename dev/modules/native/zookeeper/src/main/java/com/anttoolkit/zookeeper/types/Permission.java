package com.anttoolkit.zookeeper.types;

import java.util.*;

import org.apache.tools.ant.*;
import org.apache.zookeeper.*;
import org.apache.zookeeper.data.*;

public class Permission
{
	private static final Map<String, Integer> PERMISSIONS_MAPPING = new HashMap<String, Integer>()
	{
		{
			put("READ", ZooDefs.Perms.READ);
			put("WRITE", ZooDefs.Perms.WRITE);
			put("CREATE", ZooDefs.Perms.CREATE);
			put("DELETE", ZooDefs.Perms.DELETE);
			put("ADMIN", ZooDefs.Perms.ADMIN);
			put("ALL", ZooDefs.Perms.ALL);
		}
	};

	private String scheme;
	private String id;
	private int permissions = -1;

	public void setScheme(String scheme)
	{
		this.scheme = scheme;
	}

	public void setId(String id)
	{
	 	this.id = id;
	}

	public void setPermissions(String permissions)
	{
		this.permissions = parsePermissions(permissions);
	}

	public ACL getACL()
	{
		if (scheme == null || scheme.trim().isEmpty())
		{
			throw new BuildException("Authentication scheme is empty");
		}

		if (id == null || id.trim().isEmpty())
		{
			throw new BuildException("Authentication id is empty");
		}

		if (permissions == -1)
		{
			throw new BuildException("Permissions should be specified");
		}

		return new ACL(permissions, new Id(scheme, id));
	}

	private int parsePermissions(String permissions)
	{
		if (permissions == null || permissions.trim().isEmpty())
		{
			throw new BuildException("Permissions can't be empty");
		}

		int result = 0;

		String[] perms = permissions.split(",", -1);
		for (String perm : perms)
		{
			result = result | parsePermission(perm.trim().toUpperCase());
		}

		return result;
	}

	private int parsePermission(String permission)
	{
		Integer perm = PERMISSIONS_MAPPING.get(permission);
		if (perm == null)
		{
			throw new BuildException("Incorrect permission name specified: " + permission);
		}

		return perm;
	}
}
