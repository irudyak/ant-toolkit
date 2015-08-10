package com.anttoolkit.zookeeper.types;

import org.apache.zookeeper.data.*;

import java.util.*;

public class ACLS
{
	private List<Permission> permissions = new LinkedList<Permission>();

	public void addConfiguredPermission(Permission perm)
	{
		permissions.add(perm);
	}

	public List<ACL> getACLs()
	{
		List<ACL> acls = new LinkedList<ACL>();

		for (Permission perm : permissions)
		{
			acls.add(perm.getACL());
		}

		return acls;
	}
}
