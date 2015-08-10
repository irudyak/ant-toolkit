package com.anttoolkit.documentum.tasks.bof;

import com.documentum.fc.client.*;
import com.documentum.fc.common.*;

import org.apache.tools.ant.*;

import com.anttoolkit.documentum.common.*;

public abstract class DocAppObject
{
	public static final String OBJECT_ID_ATTRIBUTE = "r_object_id";
	public static final String NAME_ATTRIBUTE = "object_name";
	public static final String TYPE_ATTRIBUTE = "r_object_type";

	private String objectId = null;
	private String name = null;
	private String type = null;

	private DocbaseSession session = null;
	private IDfSysObject sysObj = null;

	public DocAppObject(String objectId,
						String name,
						String type,
						DocbaseSession session)
	{
		this.objectId = objectId;
		this.name = name;
		this.type = type;
		this.session = session;
	}

	public boolean equals(Object obj)
	{
		if (obj instanceof DocAppObject)
		{
			return ((DocAppObject)obj).getObjectId().equals(objectId);
		}

		return false;
	}

	public String getObjectId()
	{
		return objectId;
	}

	public String getName()
	{
		return name;
	}

	public String getType()
	{
		return type;
	}

	public abstract void checkOut() throws CheckOutException;

	public abstract void cancelCheckOut();

	public abstract void update() throws UpdateException;

	protected IDfSysObject getDfObject()
			throws BuildException
	{
		if (sysObj != null)
		{
			return sysObj;
		}

		try
		{
			return sysObj = (IDfSysObject) session.getDfObject(objectId);
		}
		catch (BuildException e)
		{
			throw new BuildException("Failed to get object of type=" + type +
					" with object_name=" + name + " and r_object_id=" + objectId +
					" from docbase", e);
		}
	}

	protected DocbaseSession getSession()
	{
		return session;
	}

	protected boolean isCheckedOutByMe(IDfSysObject obj)
			throws DfException
	{
		return obj.isCheckedOutBy(this.getSession().getUserName());
	}
}
