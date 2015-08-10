package com.anttoolkit.documentum.tasks.permissions;

import com.anttoolkit.documentum.common.*;

import org.apache.tools.ant.*;

import com.documentum.fc.client.*;
import com.documentum.fc.common.*;

public class SetOwnerTask
		extends GenericDocbaseTask
{
	private String owner = null;
	private String objectId = null;

	public void setOwner(String owner)
	{
		this.owner = owner;
	}

	public void setObjectId(String objectId)
	{
		this.objectId = objectId;
	}

	public void doWork()
			throws BuildException
	{
		if (owner == null || objectId == null)
		{
			throw new BuildException("owner and objectId are mandatory attributes");
		}

		if (objectId.length() == 16)
		{
			setObjectOwner(objectId);
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
					setObjectOwner(objectId.getId());
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

	private void setObjectOwner(String objectId)
			throws BuildException
	{
		IDfSysObject obj = (IDfSysObject)this.getDfObject(objectId);

		try
		{
			obj.setOwnerName(owner);
		}
		catch (DfException e)
		{
			throw new BuildException("Failed to set owner name for object with r_object_id=" +
					objectId, e);
		}

		this.saveDfObject(obj);
	}
}
