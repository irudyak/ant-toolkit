package com.anttoolkit.documentum.tasks.irm;

import org.apache.tools.ant.*;

import com.documentum.fc.common.*;
import com.documentum.services.irm.*;

public class RemoveObjectIRMProfileTask
		extends GenericIRMTask
{
	private String objectId = null;

	public void setObjectId(String objectId)
	{
		this.objectId = objectId;
	}

	public void doWork()
			throws BuildException
	{
		processObjectsBatch(objectId);
	}

	protected void validate()
	{
		if (objectId == null)
		{
			throw new BuildException("objectId and IRM profile should be specified");
		}
	}

	protected void processSingleObjectFromBatch(int iteration, IDfId objectId)
			throws BuildException
	{
		IIRMService irmService = getIRMService();

		try
		{
			irmService.removeObjectIRMProfile(getSession().getDfSession(), objectId);
		}
		catch (DfException e)
		{
			throw new BuildException("Failed to remove IRM profile for object r_object_id=" + this.objectId, e);
		}
	}
}
