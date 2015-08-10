package com.anttoolkit.documentum.tasks.lifecycle;

import org.apache.tools.ant.*;

import com.anttoolkit.documentum.common.*;

import com.documentum.fc.client.*;
import com.documentum.fc.common.*;

public class DemoteTask
		extends GenericDocbaseTask
{
	private String objectId = null;
	private String toState = null;
	private boolean toBase = false;

	public void setObjectId(String id)
	{
		objectId = id;
	}

	public void setToState(String state)
	{
		toState = state;
	}

	public void setToBase(boolean toBase)
	{
		this.toBase = toBase;
	}

	public void doWork() throws BuildException
	{
		processObjectsBatch(objectId);
	}

	protected void validate()
	{
		if (objectId == null)
		{
			throw new BuildException("objectId should be specified");
		}
	}

	protected void processSingleObjectFromBatch(int iteration, IDfId objectId)
			throws BuildException
	{
		IDfPersistentObject obj = getDfObject(objectId);
		if (!(obj instanceof IDfSysObject))
		{
			throw new BuildException("Object " + objectId + " is not an instance of IDfSysObject");
		}

		IDfSysObject sysObj = (IDfSysObject)obj;

		try
		{
			sysObj.demote(toState, toBase);
		}
		catch (DfException e)
		{
			throw new BuildException("Failed to demote object " + objectId, e);
		}
	}
}
