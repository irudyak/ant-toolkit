package com.anttoolkit.documentum.tasks.folder;

import org.apache.tools.ant.*;

import com.documentum.fc.client.*;
import com.documentum.fc.common.*;

import com.anttoolkit.documentum.common.*;

public class UnlinkFromFolderTask extends GenericDocbaseTask
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
			throw new BuildException("Can't unlink object r_object_id=" + objectId + " from folder='" + folder + "', because it's not of type IDfSysObject");
		}

		IDfSysObject sysObj = (IDfSysObject)obj;

		try
		{
			sysObj.unlink(folder);
			sysObj.save();
		}
		catch (DfException e)
		{
			throw new BuildException("Failed to unlink object r_object_id=" + objectId + " from folder='" + folder + "'", e);
		}
	}
}
