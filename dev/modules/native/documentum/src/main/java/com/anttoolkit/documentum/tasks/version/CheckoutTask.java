package com.anttoolkit.documentum.tasks.version;

import org.apache.tools.ant.*;

import com.documentum.fc.client.*;
import com.documentum.fc.common.*;

import com.anttoolkit.documentum.common.*;

public class CheckoutTask extends GenericDocbaseTask
{
	private String objectId = null;

	public void setObjectId(String objectId)
	{
		this.objectId = objectId;
	}

	public void doWork() throws BuildException
	{
		processObjectsBatch(objectId);
	}

	protected void validate()
	{
		if (objectId == null || objectId.trim().length() == 0)
		{
			throw new BuildException("Object id should be specified");
		}
	}

	protected void processSingleObjectFromBatch(int iteration, IDfId objectId)
			throws BuildException
	{
		IDfPersistentObject obj = getDfObject(objectId);
		if (!(obj instanceof IDfSysObject))
		{
			throw new BuildException("Object with r_object_id=" + objectId.getId() + " is not an instance of IDfSysObject");
		}

		DocbaseSession.checkoutDfObject((IDfSysObject)obj);
	}
}
