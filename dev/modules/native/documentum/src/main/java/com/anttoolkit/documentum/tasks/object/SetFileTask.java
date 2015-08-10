package com.anttoolkit.documentum.tasks.object;

import java.util.*;
import java.util.concurrent.*;

import com.documentum.com.*;
import com.documentum.fc.client.*;
import com.documentum.fc.common.*;
import com.documentum.operations.*;
import org.apache.tools.ant.*;

import com.anttoolkit.documentum.common.*;


public class SetFileTask
	extends GenericDocbaseTask
{
	private String objectId = null;
	private String file = null;
	private String contentType = null;

	private static Map<String, String> fileContentTypes = new ConcurrentHashMap<String, String>();

	public void setObjectId(String objectId)
	{
		this.objectId = objectId;
	}

	public void setFile(String file)
	{
		this.file = getFileFullPath(file);
	}

	public void setContentType(String type)
	{
		contentType = type;
	}

	public void doWork()
			throws BuildException
	{
		if (objectId == null)
		{
			return;
		}

		IDfSysObject sysObj = null;

		try
		{
			IDfPersistentObject obj = getDfObject(objectId);
			if (!(obj instanceof IDfSysObject))
			{
				if (failOnError())
				{
					throw new BuildException("Can't change object file for object with id='" + objectId +
						"' because object is not an instance of the IDfSysObject");
				}

				return;
			}

			sysObj = (IDfSysObject)obj;
		}
		catch (BuildException e)
		{
			if (failOnError())
			{
				throw e;
			}

			log("failed to get object " + objectId + " from docbase", e, Project.MSG_WARN);

			return;
		}

		setDfObjectContent(sysObj);
	}

	private String getFileContentType()
	{
		int index = file.lastIndexOf('.');
		if (index == -1)
		{
			return DocbaseObject.UNKNOWN_CONTENT_TYPE;
		}

		String fileExtension = file.substring(index + 1);

		String contentType = fileContentTypes.get(fileExtension);
		if (contentType != null)
		{
			return contentType;
		}

		try
		{
			IDfClientX clientx = new DfClientX();
			IDfFormatRecognizer recognizer = clientx.getFormatRecognizer(getSession().getDfSession(), file, "");

			contentType = recognizer.getDefaultSuggestedFileFormat() != null ? recognizer.getDefaultSuggestedFileFormat() : DocbaseObject.UNKNOWN_CONTENT_TYPE;
			fileContentTypes.put(fileExtension, contentType);

			return contentType;
		}
		catch (DfException e)
		{
			throw new BuildException("Failed to get content format for file \"" + file + "\"", e);
		}
	}

	private void setDfObjectContent(IDfSysObject obj)
			throws BuildException
	{
		removeDfObjectContent(obj);

		if (file == null || file.trim().length() == 0 ||
			file.trim().toLowerCase().equals("null"))
		{
			return;
		}

		try
		{
			String contentType = this.contentType != null ? this.contentType : getFileContentType();

			obj.setFileEx(getFileFullPath(file), contentType, 0, null);
			obj.save();
		}
		catch (DfException e)
		{
			String errorMsg = "Failed to set content file \"" + file + "\" for object " + objectId + "\r\n" + e.toString();

			if (failOnError())
			{
				throw new BuildException(errorMsg, e);
			}

			log(errorMsg, e, Project.MSG_INFO);
		}
	}

	private void removeDfObjectContent(IDfSysObject obj)
			throws BuildException
	{
		try
		{
			if (obj.getContentSize() == 0 || obj.isNew())
			{
				return;
			}

			obj.removeContent(0);
			obj.setContentType("");
		}
		catch (DfException e)
		{
			String errorMsg = "Failed to remove content for object " + objectId + "\r\n" + e.toString();

			if (failOnError())
			{
				throw new BuildException(errorMsg, e);
			}

			log(errorMsg, e, Project.MSG_INFO);
		}
	}
}
