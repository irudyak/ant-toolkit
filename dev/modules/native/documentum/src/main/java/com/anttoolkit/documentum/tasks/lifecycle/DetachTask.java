package com.anttoolkit.documentum.tasks.lifecycle;

import com.anttoolkit.documentum.common.*;

import com.documentum.fc.client.*;
import com.documentum.fc.common.*;

import org.apache.tools.ant.*;

public class DetachTask
		extends GenericDocbaseTask
{
	private String objectId = null;

	public void setObjectId(String id)
	{
		objectId = id;
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
			sysObj.detachPolicy();
		}
		catch (DfException e)
		{
			throw new BuildException("Failed to detach lifecycle " +
					"from object " + objectId, e);
		}
	}
}
