package com.anttoolkit.documentum.tasks.permissions;

import java.util.*;

import org.apache.tools.ant.*;

import com.documentum.fc.common.*;
import com.documentum.fc.client.*;

import com.anttoolkit.documentum.common.*;

public class AclTask
		extends GenericDocbaseTask
{
	private String objectId = null;
	private String aclName = null;
	private String aclDomain = null;
	private int aclClass = 3; // public ACL by default
	private String description = null;

	private List<PermitAction> actions = new LinkedList<PermitAction>();

	public void setObjectId(String objectId)
	{
		this.objectId = objectId;
	}

	public void setAclName(String name)
	{
		aclName = name;
	}

	public void setAclDomain(String domain)
	{
		aclDomain = domain;
	}

	public void setAclClass(int aclClass)
	{
		this.aclClass = aclClass;
	}

	public void setDescription(String description)
	{
		this.description = description;
	}

	public void addConfiguredGrant(PermitAction action)
	{
		action.markActionType(true);
		actions.add(action);
	}

	public void addConfiguredRevoke(PermitAction action)
	{
		action.markActionType(false);
		actions.add(action);
	}

	public void doWork()
			throws BuildException
	{
		if (objectId == null && aclName == null)
		{
			throw new BuildException("Acl name or objectId should be specified");
		}

		IDfACL acl = objectId != null ? (IDfACL)getDfObject(objectId) : (IDfACL)createDfObject("dm_acl");

		try
		{
			if (aclName != null)
			{
				acl.setObjectName(aclName);
			}

			if (aclDomain != null)
			{
				acl.setDomain(aclDomain);
			}

			if (description != null)
			{
				acl.setDescription(description);
			}

			if (acl.getACLClass() != aclClass)
			{
				acl.setACLClass(aclClass);
			}

			for (PermitAction action : actions)
			{
				action.execute(acl);
			}

			if (acl.isNew() || acl.isDirty())
			{
				acl.save();
			}
		}
		catch (DfException e)
		{
			if (objectId != null)
			{
				throw new BuildException("Failed to update ACL", e);
			}
			else
			{
				throw new BuildException("Failed to create ACL", e);
			}
		}
	}
}
