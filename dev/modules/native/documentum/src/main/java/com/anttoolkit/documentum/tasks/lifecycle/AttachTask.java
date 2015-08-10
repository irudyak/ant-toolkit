package com.anttoolkit.documentum.tasks.lifecycle;

import com.documentum.fc.client.IDfPersistentObject;
import com.documentum.fc.client.IDfSysObject;
import com.documentum.fc.common.*;

import org.apache.tools.ant.*;

import com.anttoolkit.documentum.common.*;

public class AttachTask
		extends GenericDocbaseTask
{
	private String objectId = null;
	private String lifecycle = null;
	private IDfId lifecycleId = null;
	private String state = "0";

	public void setObjectId(String id)
	{
		objectId = id;
	}

	public void setLifecycle(String lifecycle)
	{
		this.lifecycle = lifecycle;
	}

	public void setState(String state)
	{
		this.state = state;
	}

	public void doWork() throws BuildException
	{
		lifecycleId = DfId.isObjectId(lifecycle) ? new DfId(lifecycle) :
				LifecycleHelper.getLifecycleId(getSession(), lifecycle);

		processObjectsBatch(objectId);
	}

	protected void validate()
	{
		if (objectId == null || lifecycle == null)
		{
			throw new BuildException("objectId and lifecycle should be specified");
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
			sysObj.attachPolicy(lifecycleId, state, "");
		}
		catch (DfException e)
		{
			throw new BuildException("Failed to attach lifecycle '" + lifecycle + "' " +
					"to object " + objectId, e);
		}
	}
}
