package com.anttoolkit.documentum.tasks.object;

import com.documentum.fc.client.*;
import org.apache.tools.ant.*;

import com.documentum.fc.common.*;

import com.anttoolkit.documentum.common.*;

public class DestroyObjectTask
		extends GenericDocbaseTask
{
	private String objectId = null;
	private boolean allVersions = false;

	public void setObjectId(String objectId)
	{
		this.objectId = objectId;
	}

	public void setAllVersions(boolean allVersions)
	{
		this.allVersions = allVersions;
	}

	public void doWork() throws BuildException
	{
		processObjectsBatch(objectId);
	}

	protected void validate()
	{
		if (objectId == null)
		{
			throw new BuildException("Object id should be specified");
		}
	}

	protected void processSingleObjectFromBatch(int iteration, IDfId objectId)
			throws BuildException
	{
		IDfPersistentObject obj = getDfObject(objectId);

		try
		{
			if (!allVersions || !(obj instanceof IDfSysObject))
			{
				obj.destroy();
				return;
			}

			IDfSysObject sysObj = (IDfSysObject)obj;
			sysObj.destroyAllVersions();
		}
		catch (DfException e)
		{
			if (failOnError())
			{
				throw new BuildException("Failed to destroy object r_object_id=" + objectId.getId(), e);
			}

			log("Failed to destroy object r_object_id=" + objectId.getId(), e, Project.MSG_WARN);
		}
	}

}
