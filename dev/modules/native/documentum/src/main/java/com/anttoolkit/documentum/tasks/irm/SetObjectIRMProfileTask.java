package com.anttoolkit.documentum.tasks.irm;

import org.apache.tools.ant.*;

import com.documentum.fc.common.*;
import com.documentum.services.irm.*;
import com.documentum.fc.client.*;

public class SetObjectIRMProfileTask
		extends GenericIRMTask
{
	private String objectId = null;
	private String profile = null;

	public void setObjectId(String objectId)
	{
		this.objectId = objectId;
	}

	public void setIrmProfile(String profile)
	{
		this.profile = profile;
	}

	public void doWork()
			throws BuildException
	{
		processObjectsBatch(objectId);
	}

	protected void validate()
	{
		if (objectId == null || profile == null)
		{
			throw new BuildException("objectId and IRM profile should be specified");
		}
	}

	protected void processSingleObjectFromBatch(int iteration, IDfId objectId)
			throws BuildException
	{
		IDfPersistentObject obj = getDfObject(objectId);
		if (!(obj instanceof IDfSysObject))
		{
			throw new BuildException("Can't apply IRM profile to object r_object_id=" + objectId + ", because it's not of type IDfSysObject");
		}

		IDfSysObject irmProfile = getIRMProfileByName(profile);
		IIRMService irmService = getIRMService();

		try
		{
			irmService.protectDocument((IDfSysObject)obj, null, irmProfile);
		}
		catch (DfException e)
		{
			throw new BuildException("Failed to protect document r_object_id=" + objectId + " with IRM profile '" + profile + "'", e);
		}

		try
		{
			obj.save();
		}
		catch (DfException e)
		{
			throw new BuildException("Failed to save document r_object_id=" + objectId + " after IRM protection with profile '" + profile + "'", e);
		}
	}
}

