package com.anttoolkit.documentum.tasks.object;

import java.text.*;
import java.util.*;
import java.util.concurrent.*;

import org.apache.tools.ant.*;

import com.documentum.fc.client.*;
import com.documentum.fc.common.*;

import com.anttoolkit.documentum.common.*;

public class GetFileTask
		extends GenericDocbaseTask
{
	private static final String DQL_GET_FORMAT_EXTENSION = "select dos_extension from dm_format where name=''{0}''";

	private static Map<String, String> formatExtensions = new ConcurrentHashMap<String, String>();

	private String objectId = null;
	private String folder = null;
	private String fileName = null;
	private String fileNameProperty = null;
	private boolean resolveFileExtension = true;

	public void setObjectId(String objectId)
	{
		this.objectId = objectId;
	}

	public void setFolder(String folder)
	{
		this.folder = folder;
	}

	public void setFile(String name)
	{
		fileName = name;
	}

	public void setUseObjectPropertyAsFileName(String property)
	{
		fileNameProperty = property;
	}

	public void setResolveFileExtension(boolean resolve)
	{
		resolveFileExtension = resolve;
	}

	public void doWork() throws BuildException
	{
		processObjectsBatch(objectId);
	}

	protected void validate()
	{
		if (objectId == null)
		{
			return;
		}

		if (fileName == null && fileNameProperty == null)
		{
			throw new BuildException("Either fileName or fileNameProperty should be specified");
		}
	}

	protected void processSingleObjectFromBatch(int iteration, IDfId objectId)
			throws BuildException
	{
		IDfSysObject sysObj = null;

		try
		{
			IDfPersistentObject obj = getDfObject(objectId);
			if (!(obj instanceof IDfSysObject))
			{
				if (failOnError())
				{
					throw new BuildException("Can't get object file for object with id='" + objectId +
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

			log("Failed to process " + objectId + " object from batch", e, Project.MSG_WARN);

			return;
		}

		try
		{
			if (sysObj.getContentSize() == 0)
			{
				String msg = "Object " + objectId + " doesn't have content";

				if (!failOnError())
				{
					log(msg, Project.MSG_WARN);
					return;
				}

				throw new BuildException(msg);
			}
		}
		catch (DfException e)
		{
			throw new BuildException("Failed to verify object " + objectId + " content size", e);
		}

		String fileFullPath = generateFileFullPath(sysObj);

		try
		{
			sysObj.getFile(fileFullPath);
		}
		catch (DfException e)
		{
			throw new BuildException("Failed to get file for object " + objectId, e);
		}
	}

	private String generateFileFullPath(IDfSysObject sysObj)
	{
		String fullPath = "";

		if (folder != null)
		{
			fullPath = folder;
			fullPath = fullPath.trim().replace("\\", "/");

			if (fullPath.charAt(fullPath.length() - 1) != '/')
			{
				fullPath = fullPath + "/";
			}
		}

		if (fileName != null)
		{
			return this.getFileFullPath(fullPath + fileName);
		}

		if (fileNameProperty != null)
		{
			try
			{
				fullPath = fullPath + sysObj.getString(fileNameProperty);
				fullPath = fullPath.trim();
			}
			catch (DfException e)
			{
				throw new BuildException("Failed to get object property '" + fileNameProperty + "' value", e);
			}
		}

		if (resolveFileExtension)
		{
			String extension = getFormatExtension(sysObj).toLowerCase();

			if (!fullPath.endsWith(extension))
			{
				fullPath = fullPath + extension;
			}
		}

		return this.getFileFullPath(fullPath);
	}

	private String getFormatExtension(IDfSysObject sysObj)
	{
		try
		{
			String contentType = sysObj.getContentType();
			if (formatExtensions.containsKey(contentType))
			{
				return formatExtensions.get(contentType);
			}


			String query = MessageFormat.format(DQL_GET_FORMAT_EXTENSION, contentType);
			String formatExtension = DqlHelper.getStringParamFromFirstString(getSession(), query);

			formatExtensions.put(contentType, formatExtension);

			return formatExtension;
		}
		catch (DfException e)
		{
			throw new BuildException("Failed to get format extension for object", e);
		}
	}
}
