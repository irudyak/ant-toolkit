package com.anttoolkit.documentum.tasks.permissions;

import com.anttoolkit.documentum.common.*;

import java.util.*;

import org.apache.tools.ant.*;

import com.documentum.fc.client.*;
import com.documentum.fc.common.*;

public class SetPermitTask
		extends GenericDocbaseTask
{
	private String objectId = null;
	private boolean resetFlag = false;
	private String owner = null;
	private Map<String, UserPermit> userPermits = new HashMap<String, UserPermit>();

	public void addConfiguredUser(UserPermit permit)
	{
		if (!userPermits.containsKey(permit.getName()))
		{
			userPermits.put(permit.getName(), permit);
			return;
		}

		UserPermit _permit = userPermits.get(permit.getName());
		if (_permit.getPermit() < permit.getPermit())
		{
			_permit.setIntPermit(permit.getPermit());
		}

		_permit.appendXpermits(permit.getXpermit());
	}

	public void setObjectId(String objectId)
	{
		this.objectId = objectId;
	}

	public void setResetACL(boolean resetFlag)
	{
		this.resetFlag = resetFlag;
	}

	public void setOwner(String owner)
	{
		this.owner = owner;
	}

	public void doWork()
			throws BuildException
	{
		if (objectId == null)
		{
			throw new BuildException("objectId attribute is mandatory");
		}

		if (objectId.length() == 16)
		{
			setUserPermits((IDfSysObject)this.getDfObject(objectId));
			return;
		}

		IDfCollection coll = null;
		try
		{
			try
			{
				coll = DqlHelper.executeReadQuery(this.getSession(), objectId);
			}
			catch (DfException e)
			{
				throw new BuildException("Failed to execute query \"" + objectId + "\" to get id's collection", e);
			}

			try
			{
				while (coll.next())
				{
					IDfId objectId = coll.getValueAt(0).asId();
					setUserPermits((IDfSysObject)this.getDfObject(objectId.getId()));
				}
			}
			catch (DfException e)
			{
				throw new BuildException("Failed to perform next interation on IDfCollection object to get object id", e);
			}
		}
		finally
		{
			DqlHelper.closeCollection(coll);
		}
	}

	private void setUserPermits(IDfSysObject sysObj)
			throws BuildException
	{
		if (resetFlag)
		{
			AclHelper.resetObjectACL(sysObj);
		}

		if (owner != null)
		{
			try
			{
				sysObj.setOwnerName(owner);
			}
			catch (DfException e)
			{
				throw new BuildException("Failed to set object owner", e);
			}
		}

		for (String userName : userPermits.keySet())
		{
			UserPermit userPermit = userPermits.get(userName);
			AclHelper.setPermitForUser(sysObj, userName, userPermit.getPermit(), userPermit.getXpermit());
		}

		this.saveDfObject(sysObj);
	}
}
