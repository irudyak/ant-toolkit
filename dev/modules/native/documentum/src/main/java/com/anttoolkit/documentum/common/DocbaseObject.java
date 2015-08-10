package com.anttoolkit.documentum.common;

import com.documentum.fc.client.*;
import com.documentum.fc.common.*;
import com.documentum.com.*;
import com.documentum.operations.*;

import java.util.*;
import java.text.*;
import java.util.concurrent.*;

import org.apache.tools.ant.*;

public class DocbaseObject
{
	public static final String UNKNOWN_CONTENT_TYPE = "unknown";
	private static final String DQL_GET_FOLDER_ID_BY_PATH = "select r_object_id from dm_folder where any r_folder_path=''{0}''";
	private static final int FOLDER_IDS_CACHE_MAX_SIZE = 2048;

	private boolean useCheckinOnSave = false;

	private String type = null;
	private String objectId = null;
	private String folder = null;
	private String contentFile = null;
	private String contentType = null;

	private IDfType dfType = null;

	private List<DocbaseObjectProperty> properties = new LinkedList<DocbaseObjectProperty>();
	private DocbaseSession session = null;

	private String lastCommitedObjectId = null;

	private static Map<String, String> folderIdsCache = new ConcurrentHashMap<String, String>();

	private static Map<String, Map<String, IDfAttr>> types = new ConcurrentHashMap<String, Map<String, IDfAttr>>();


	public void setCheckinOperationFlag()
	{
		useCheckinOnSave = true;
	}

	public void addProperty(DocbaseObjectProperty property)
	{
		properties.add(property);
	}

	public DocbaseObjectProperty getProperty(String name)
	{
		for (DocbaseObjectProperty property : properties)
		{
			if (property.getName().equals(name))
			{
				return property;
			}
		}

		return null;
	}

	public void setContentFile(String file)
	{
		contentFile = file;
	}

	public String getContentFile()
	{
		return contentFile;
	}

	public void setContentType(String type)
	{
		contentType = type;
	}

	public String getContentType()
	{
		return contentType;
	}

	public void clearProperties()
	{
		properties.clear();
	}

	public void clear()
	{
		type = null;
		objectId = null;
		folder = null;
		dfType = null;

		clearProperties();
	}

	public void setType(String type)
	{
		this.type = type;

		try
		{
			if (dfType != null && !this.type.equals(dfType.getName()))
			{
				dfType = null;
			}
		}
		catch (DfException e)
		{
			throw new BuildException("Failed to get type name from IDfType object", e);
		}
	}

	public String getType()
	{
		return type;
	}

	public String getLastCommitedObjectId()
	{
		return lastCommitedObjectId;
	}

	public void setObjectId(String objectId)
	{
		this.objectId = objectId;
	}

	public String getObjectId()
	{
		return objectId;
	}

	public void setFolder(String folder)
	{
		this.folder = folder;
	}

	public void doWork()
			throws BuildException
	{
		if (objectId != null && type != null)
		{
			throw new BuildException("You should specify objectId or type arguments, but not both");
		}

		if ((objectId == null || objectId.trim().length() == 0 ||
			objectId.trim().equals(DfId.DF_NULLID_STR)) &&
			(type == null || type.trim().length() == 0))
		{
			throw new BuildException("Object objectId or type argument shoul be specified");
		}

		//create new object
		if (type != null)
		{
			commitObject(createDfObject());
			return;
		}

		//update single object
		if (objectId.length() == 16)
		{
			commitObject(getDfObject(objectId));
			return;
		}

		//update multiple objects
		commitBatch(objectId);
	}

	public void setSession(DocbaseSession session)
	{
		this.session = session;
	}

	private IDfPersistentObject createDfObject()
			throws BuildException
	{
		if (session == null)
		{
			throw new BuildException("DocbaseSession wasn't set for DocbaseObject");
		}

		initObjectPropertiesMetadata();

		try
		{
			IDfPersistentObject obj =  session.createDfObject(type);
			objectId = obj.getObjectId().toString();
			return obj;
		}
		catch (DfException e)
		{
			throw new BuildException("Failed to get object id for created object", e);
		}
	}

	private IDfPersistentObject getDfObject(String objectId)
			throws BuildException
	{
		if (session == null)
		{
			throw new BuildException("DocbaseSession wasn't set for DocbaseObject");
		}

		IDfPersistentObject obj = session.getDfObject(objectId);

		try
		{
			this.setType(obj.getType().getName());
		}
		catch (DfException e)
		{
			throw new BuildException("Failed to get object type name", e);
		}

		initObjectPropertiesMetadata();

		return obj;
	}

	private void commitObject(IDfPersistentObject obj)
			throws BuildException
	{
		for (DocbaseObjectProperty prop : properties)
		{
			Object value = prop.resolvePropertyValue(session);

			try
			{
				switch (prop.getDataType())
				{
					case IDfType.DF_BOOLEAN:
						if (prop.isRepeating())
						{
							obj.appendBoolean(prop.getName(), (Boolean) value);
						}
						else
						{
							obj.setBoolean(prop.getName(), (Boolean) value);
						}
						break;
					case IDfType.DF_DOUBLE:
						if (prop.isRepeating())
						{
							obj.appendDouble(prop.getName(), (Double) value);
						}
						else
						{
							obj.setDouble(prop.getName(), (Double) value);
						}
						break;
					case IDfType.DF_ID:
						if (prop.isRepeating())
						{
							obj.appendId(prop.getName(), (IDfId) value);
						}
						else
						{
							obj.setId(prop.getName(), (IDfId) value);
						}
						break;
					case IDfType.DF_INTEGER:
						if (prop.isRepeating())
						{
							obj.appendInt(prop.getName(), (Integer) value);
						}
						else
						{
							obj.setInt(prop.getName(), (Integer) value);
						}
						break;
					case IDfType.DF_STRING:
						if (prop.isRepeating())
						{
							obj.appendString(prop.getName(), (String) value);
						}
						else
						{
							obj.setString(prop.getName(), (String) value);
						}
						break;
					case IDfType.DF_TIME:
						if (prop.isRepeating())
						{
							obj.appendTime(prop.getName(), (IDfTime) value);
						}
						else
						{
							obj.setTime(prop.getName(), (IDfTime) value);
						}
						break;
				}
			}
			catch (DfException e)
			{
				throw new BuildException("Failed to set object property " + prop.getName(), e);
			}
		}

		linkDfObject(obj);
		setDfObjectContent(obj);
		saveDfObject(obj);
	}

	private void commitBatch(String query)
			throws BuildException
	{
		if (session == null)
		{
			throw new BuildException("DocbaseSession wasn't set for DocbaseObject");
		}

		IDfCollection coll = null;
		try
		{
			try
			{
				coll = DqlHelper.executeReadQuery(session, query);
			}
			catch (DfException e)
			{
				throw new BuildException("Failed to execute query \"" + query + "\" to get id's collection", e);
			}

			try
			{
				while (coll.next())
				{
					IDfId objectId = coll.getValueAt(0).asId();
					commitObject(getDfObject(objectId.getId()));
				}
			}
			catch (DfException e)
			{
				throw new BuildException("Failed to perform next interation on IDfCollection object to get object id", e);
			}
		}
		finally
		{
			DqlHelper.closeCollection(coll);
		}
	}

	private void linkDfObject(IDfPersistentObject obj)
			throws BuildException
	{
		if (folder == null)
		{
			return;
		}

		if (!(obj instanceof IDfSysObject))
		{
			throw new BuildException("Object of type " + type + " isn't inherited from dm_sysobject " +
					"so it couldn't be link to any folder");
		}

		try
		{
			((IDfSysObject)obj).link(getFolderId(folder));
		}
		catch (DfException e)
		{
			throw new BuildException("Failed to link object to folder: " + folder, e);
		}
	}

	private void setDfObjectContent(IDfPersistentObject obj)
			throws BuildException
	{
		if (!(obj instanceof IDfSysObject) ||
			contentFile == null || contentFile.trim().length() == 0)
		{
			return;
		}

		removeDfObjectContent((IDfSysObject)obj);

		if (contentFile.trim().toLowerCase().equals("null"))
		{
			return;
		}

		try
		{
			String contentType = this.contentType != null ? this.contentType : getFileContentType(contentFile);
			((IDfSysObject)obj).setFileEx(contentFile, contentType, 0, null);
		}
		catch (DfException e)
		{
			throw new BuildException("Failed to set content file \"" + contentFile + "\" for object " + objectId, e);
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
			throw new BuildException("Failed to remove content for object " + objectId, e);
		}
	}

	private void saveDfObject(IDfPersistentObject obj)
			throws BuildException
	{
		if (!useCheckinOnSave || !(obj instanceof IDfSysObject))
		{
			lastCommitedObjectId = DocbaseSession.saveDfObject(obj);
		}
		else
		{
			lastCommitedObjectId = DocbaseSession.checkinDfObject((IDfSysObject)obj);
		}
	}

	private void initObjectPropertiesMetadata()
			throws BuildException
	{
		if (properties.isEmpty())
		{
			return;
		}

		for (DocbaseObjectProperty prop : properties)
		{
			if (prop.isMetadataInitialized())
			{
				continue;
			}

			try
			{
				IDfAttr dfAttr = getDfAttr(prop.getName());
				if (dfAttr == null)
				{
					throw new BuildException("Attribute '" + prop.getName() + "' for type '" + getType() + "' doesn't exist");
				}

				prop.setMetadata(dfAttr.getDataType(), dfAttr.getLength(), dfAttr.isRepeating());
			}
			catch (DfException e)
			{
				throw new BuildException("Error occured while trying to get metadata for attribute " + prop.getName());
			}
		}
	}

	private IDfType getDfType()
			throws DfException
	{
		if (dfType != null)
		{
			return dfType;
		}

		dfType = session.getDfSession().getType(type);

		return dfType;
	}

	private IDfAttr getDfAttr(String attrName)
			throws DfException
	{
		if (!types.containsKey(type))
		{
			types.put(type, new HashMap<String, IDfAttr>());
		}

		Map<String, IDfAttr> cachedAttributes = types.get(type);
		if (cachedAttributes.containsKey(attrName))
		{
			return cachedAttributes.get(attrName);
		}

		IDfAttr dfAttr = this.getDfType().getTypeAttr(this.getDfType().findTypeAttrIndex(attrName));
		cachedAttributes.put(attrName, dfAttr);

		return dfAttr;
	}

	private String getFileContentType(String fileName)
	{
		if(fileName == null)
		{
			return null;
		}

		try
		{
			IDfClientX clientx = new DfClientX();
			IDfFormatRecognizer recognizer = clientx.getFormatRecognizer(session.getDfSession(), fileName, "");

			return recognizer.getDefaultSuggestedFileFormat() != null ? recognizer.getDefaultSuggestedFileFormat() : UNKNOWN_CONTENT_TYPE;
		}
		catch (DfException e)
		{
			throw new BuildException("Failed to get content format for file \"" + fileName + "\"", e);
		}
	}

	private String getFolderId(String folderExpression)
	{
		if (folderExpression == null || folderExpression.trim().length() == 0)
		{
			return null;
		}

		if (folderIdsCache.containsKey(folderExpression))
		{
			return folderIdsCache.get(folderExpression);
		}

		if (DfId.isObjectId(folderExpression))
		{
			return folderExpression;
		}

		if (!folderExpression.contains("/"))
		{
			throw new BuildException("Incorrect folder expression \"" + folderExpression + "\" specified to link object");
		}

		String dqlQuery = MessageFormat.format(DQL_GET_FOLDER_ID_BY_PATH, folderExpression);
		String folderId;

		try
		{
			folderId = DqlHelper.getStringParamFromFirstString(session, dqlQuery);
		}
		catch (DfException e)
		{
			throw new BuildException("Failed to get folderId for folder \"" + folderExpression + "\"", e);	
		}

		if (folderIdsCache.size() == FOLDER_IDS_CACHE_MAX_SIZE)
		{
			folderIdsCache.clear();
		}

		folderIdsCache.put(folderExpression, folderId);

		return folderId;
	}
}
