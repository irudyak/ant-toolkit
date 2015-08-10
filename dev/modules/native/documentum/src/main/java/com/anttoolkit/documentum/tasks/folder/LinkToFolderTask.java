package com.anttoolkit.documentum.tasks.folder;

import org.apache.tools.ant.*;

import com.documentum.fc.client.*;
import com.documentum.fc.common.*;

import com.anttoolkit.documentum.common.*;

public class LinkToFolderTask extends GenericDocbaseTask
{
	private String objectId = null;
	private String folder = null;

	public void setObjectId(String objectId)
	{
		this.objectId = objectId;
	}

	public void setFolder(String folder)
	{
		this.folder = folder;
	}

	public void doWork()
			throws BuildException
	{
		processObjectsBatch(objectId);
	}

	protected void validate()
	{
		if (objectId == null || folder == null)
		{
			throw new BuildException("objectId and folder should be specified");
		}
	}

	protected void processSingleObjectFromBatch(int iteration, IDfId objectId)
			throws BuildException
	{
		IDfPersistentObject obj = getDfObject(objectId);
		if (!(obj instanceof IDfSysObject))
		{
			throw new BuildException("Can't link object r_object_id=" + objectId + " to folder='" + folder + "', because it's not of type IDfSysObject");
		}

		IDfSysObject sysObj = (IDfSysObject)obj;

		try
		{
			sysObj.link(folder);
			sysObj.save();
		}
		catch (DfException e)
		{
			throw new BuildException("Failed to link object r_object_id=" + objectId + " to folder='" + folder + "'", e);
		}
	}
}
