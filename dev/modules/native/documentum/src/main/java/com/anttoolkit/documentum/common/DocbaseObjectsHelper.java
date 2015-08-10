package com.anttoolkit.documentum.common;

import java.text.*;
import java.util.*;

import org.apache.tools.ant.*;

import com.documentum.fc.common.*;
import com.documentum.fc.client.*;

public class DocbaseObjectsHelper
{
	private static final String DOCBASE_VERSION_FORMAT = "{0}.{1}";

	private static final String CURRENT_VERSION_LABEL = "CURRENT";

	private static final String DQL_GET_OBJECT_ID_BY_OBJECT_NAME = "select r_object_id" +
			" from {0} where object_name=''{1}''";

	private static final String DQL_GET_OBJECT_NAME_BY_OBJECT_ID = "select object_name" +
			" from {0} where r_object_id=''{1}''";

	private static final String DQL_GET_CURRENT_OBJECT_ID = "select r_object_id" +
			" from dm_sysobject" +
			" where i_chronicle_id=''{0}''";

	private static final String DQL_MOVE_OBJECT = "update dm_sysobject object " +
			"move ''{0}'' where r_object_id in ({1})";

	private static final String DQL_REMOVE_OBJECTS = "delete {0} (all) object where r_object_id in ({1})";

	private static final String DQL_REMOVE_UNVERSION_OBJECTS = "delete {0} object where r_object_id in ({1})";

	private static final String DQL_REMOVE_ALL_OBJECT_VERSIONS = "delete {0} (all) object where i_chronicle_id=''{1}''";

	private static final String DQL_GET_CHRONICLE_ID_BY_OBJECT_ID = "select i_chronicle_id" +
			" from dm_sysobject" +
			" where r_object_id=''{0}''";

	private static final String DQL_GET_OBJECT_ID_FOR_LAST_OBJECT_VERSION = "select r_object_id" +
			" from dm_sysobject" +
			" where i_chronicle_id in (" +
			" select i_chronicle_id from dm_sysobject (all) " +
			" where r_object_id=''{0}'')";

	private static final String DQL_GET_DOCBASE_VERSION = "select si_major, si_minor from dt_docbase_version";

	private static final String DQL_DOES_FOLDER_CONTAINS_ANY_OBJECTS_1 = "select r_object_id" +
			" from dm_sysobject where folder(ID(''{0}'')) enable(return_top 1)";

	private static final String DQL_DOES_FOLDER_CONTAINS_ANY_OBJECTS_2 = "select r_object_id" +
			" from dm_sysobject where folder(''{0}'') enable(return_top 1)";

	private static final String DQL_GET_FOLDER_ID_BY_PATH = "select r_object_id" +
			" from dm_folder where any r_folder_path=''{0}'' " +
			"enable(return_top 1)";

	private static final String DQL_IS_OBJECT_EXIST = "select r_object_id" +
			" from {0} where r_object_id=''{1}'' enable(return_top 1)";

	private static final String DQL_HAS_OBJECT_OTHER_VERSIONS = "select r_object_id" +
			" from dm_sysobject (all)" +
			" where i_chronicle_id=''{0}'' and r_object_id!=''{1}''" +
			" enable(return_top 1)";

	private static final String DQL_HAS_PERMITS_FOR_OBJECT = "select for {0} r_object_id" +
			" from dm_sysobject (all)" +
			" where r_object_id=''{1}''" +
			" enable(return_top 1)";

	private static final String DQL_GET_DOCBASE_OWNER = "select owner_name" +
			" from dm_docbase_config" +
			" enable(return_top 1)";

	private static volatile Map<String, String> docbaseOwners = new HashMap<String, String>();

	public static IDfId getObjectIdByObjectName(DocbaseSession session, String objectType, String objectName)
			throws DfException, DfEndOfCollectionException
	{
		return DqlHelper.getIdParamFromFirstString(session, MessageFormat.format(DQL_GET_OBJECT_ID_BY_OBJECT_NAME, objectType, objectName));
	}

	public static String getObjectNameByObjectId(DocbaseSession session, String objectType, IDfId objectId)
			throws DfException, DfEndOfCollectionException
	{
		return DqlHelper.getStringParamFromFirstString(session, MessageFormat.format(DQL_GET_OBJECT_NAME_BY_OBJECT_ID, objectType, objectId.toString()));
	}

	public static String getCurrentObjectId(DocbaseSession session, String chronicleId)
			throws DfException, DfEndOfCollectionException
	{
		return DqlHelper.getStringParamFromFirstString(session, MessageFormat.format(DQL_GET_CURRENT_OBJECT_ID, chronicleId));
	}

	public static void moveObjectsToFolder(DocbaseSession session, String folderPath, String... objectIds)
			throws DfException
	{
		if (objectIds == null || objectIds.length == 0)
		{
			return;
		}

		StringBuffer ids = new StringBuffer();
		for (String objectId : objectIds)
		{
			if (ids.length() != 0)
			{
				ids.append(",");
			}

			ids.append("'").append(objectId).append("'");
		}

		DqlHelper.executeQuery(session, MessageFormat.format(DQL_MOVE_OBJECT, folderPath, ids.toString()));
	}

	public static void removeObjects(DocbaseSession session, String type, String... ids)
			throws DfException
	{
		if (ids == null || ids.length == 0)
		{
			return;
		}

		if (ConversionHelper.isEmptyString(type))
		{
			type = "dm_sysobject";
		}

		StringBuffer objectIds = new StringBuffer();

		for (String id : ids)
		{
			if (objectIds.length() > 0)
			{
				objectIds.append(",");
			}

			objectIds.append("'").append(id).append("'");
		}

		if ("dm_sysobject".equals(type) ||
			DocbaseTypesHelper.inheritsFromType(session, type, "dm_sysobject"))
		{
			DqlHelper.executeQuery(session, MessageFormat.format(DQL_REMOVE_OBJECTS, type, objectIds.toString()));
		}
		else
		{
			DqlHelper.executeQuery(session, MessageFormat.format(DQL_REMOVE_UNVERSION_OBJECTS, type, objectIds.toString()));
		}
	}

	public static void removeObjects(DocbaseSession session, String... ids)
			throws DfException
	{
		removeObjects(session, null, ids);
	}

	public static void removeObjects(DocbaseSession session, String type, List ids)
			throws DfException
	{
		if (ids == null || ids.size() == 0)
		{
			return;
		}

		if (ConversionHelper.isEmptyString(type))
		{
			type = "dm_sysobject";
		}

		StringBuffer objectIds = new StringBuffer();

		for (Object id : ids)
		{
			if (objectIds.length() > 0)
			{
				objectIds.append(",");
			}

			objectIds.append("'").append(id.toString()).append("'");
		}

		if ("dm_sysobject".equals(type) ||
			DocbaseTypesHelper.inheritsFromType(session, type, "dm_sysobject"))
		{
			DqlHelper.executeQuery(session, MessageFormat.format(DQL_REMOVE_OBJECTS, type, objectIds.toString()));
		}
		else
		{
			DqlHelper.executeQuery(session, MessageFormat.format(DQL_REMOVE_UNVERSION_OBJECTS, type, objectIds.toString()));
		}
	}

	public static void removeObjects(DocbaseSession session, List ids)
			throws DfException
	{
		removeObjects(session, null, ids);
	}

	public static void removeAllObjectVersions(DocbaseSession session, String objectId)
			throws DfException
	{
		removeAllObjectVersions(session, null, objectId);
	}

	public static void removeAllObjectVersions(DocbaseSession session, String type, String objectId)
			throws DfException
	{
		String chronicleId = null;

		try
		{
			chronicleId = DocbaseObjectsHelper.getChronicleIdByObjectId(session, objectId);
		}
		catch (DfEndOfCollectionException e)
		{
			return;
		}

		type = type != null ? type : "dm_sysobject";

		DqlHelper.executeQuery(session, MessageFormat.format(DQL_REMOVE_ALL_OBJECT_VERSIONS, type, chronicleId));
	}

	public static String getChronicleIdByObjectId(DocbaseSession session, String objectId)
			throws DfException, DfEndOfCollectionException
	{
		return DqlHelper.getStringParamFromFirstString(session, MessageFormat.format(DQL_GET_CHRONICLE_ID_BY_OBJECT_ID, objectId));
	}

	public static String getObjectIdForLastObjectVersion(DocbaseSession session, String concreteVersionObjectId)
			throws DfException, DfEndOfCollectionException
	{
		return DqlHelper.getStringParamFromFirstString(session, MessageFormat.format(DQL_GET_OBJECT_ID_FOR_LAST_OBJECT_VERSION, concreteVersionObjectId));
	}

	public static boolean hasCurrentVersion(IDfSysObject obj)
			throws DfException
	{
		if (obj == null)
		{
			return false;
		}

		if (obj.isNew())
		{
			return true;
		}

		int count = obj.getVersionLabelCount();
		if (count < 2)
		{
			return false;
		}

		for (int i = 0; i < count; i++)
		{
			if (CURRENT_VERSION_LABEL.equals(obj.getVersionLabel(i)))
			{
				return true;
			}
		}

		return false;
	}

	public static String getDocbaseVersion(DocbaseSession session)
			throws DfException, DfEndOfCollectionException
	{
		IDfTypedObject obj = DqlHelper.getFirstString(session, DQL_GET_DOCBASE_VERSION);
		return MessageFormat.format(DOCBASE_VERSION_FORMAT, obj.getString("si_major"), obj.getString("si_minor"));
	}

	public static boolean doesFolderContainAnyObjects(DocbaseSession session, IDfId folderId)
			throws DfException
	{
		return DqlHelper.exist(session, MessageFormat.format(DQL_DOES_FOLDER_CONTAINS_ANY_OBJECTS_1, folderId.getId()));
	}

	public static boolean doesFolderContainAnyObjects(DocbaseSession session, String folderPath)
			throws DfException
	{
		return DqlHelper.exist(session, MessageFormat.format(DQL_DOES_FOLDER_CONTAINS_ANY_OBJECTS_2, folderPath));
	}

	public static String getFolderIdByPath(DocbaseSession session, String folderPath)
			throws DfException
	{
		return DqlHelper.getStringParamFromFirstString(session, MessageFormat.format(DQL_GET_FOLDER_ID_BY_PATH, folderPath));
	}

	public static boolean isObjectCheckedOutByNotCurrentUser(DocbaseSession session, IDfPersistentObject obj)
	{
		try
		{
			if (obj == null || !(obj instanceof IDfSysObject) || obj.isNew())
			{
				return false;
			}

			IDfSysObject sysObj = (IDfSysObject)obj;

			return sysObj.isCheckedOut() && !sysObj.isCheckedOutBy(session.getUserName());
		}
		catch (DfException e)
		{
			throw new RuntimeException("Failed to verify if object check out by not current user", e);
		}
	}

	public static boolean isObjectCheckedOutByCurrentUser(DocbaseSession session, IDfPersistentObject obj)
	{
		try
		{
			if (obj == null || !(obj instanceof IDfSysObject) || obj.isNew())
			{
				return false;
			}

			IDfSysObject sysObj = (IDfSysObject)obj;

			return sysObj.isCheckedOut() && sysObj.isCheckedOutBy(session.getUserName());
		}
		catch (DfException e)
		{
			throw new RuntimeException("Failed to verify if object check out by current user", e);
		}
	}

	public static boolean isObjectExist(DocbaseSession session, String objectId, String objectType)
			throws DfException
	{
		objectType = objectType == null ? "dm_sysobject" : objectType;
		return DqlHelper.exist(session, MessageFormat.format(DQL_IS_OBJECT_EXIST, objectType, objectId));
	}

	public static boolean canCurrentUserDeleteObject(DocbaseSession session, IDfPersistentObject obj)
			throws DfException
	{
		if (!(obj instanceof IDfSysObject))
		{
			return true;
		}

		IDfSysObject sysObj = (IDfSysObject)obj;

		return !sysObj.isCheckedOut() && (sysObj.getPermit() == IDfACL.DF_PERMIT_DELETE ||
				sysObj.getXPermitNames(session.getUserName()).indexOf(IDfACL.DF_XPERMIT_DELETE_OBJECT_STR) >= 0);
	}

	public static boolean hasOtherVersions(DocbaseSession session, IDfPersistentObject obj)
			throws DfException
	{
		if (!(obj instanceof IDfSysObject))
		{
			return false;
		}

		IDfSysObject sysObj = (IDfSysObject)obj;

		return DqlHelper.exist(session, MessageFormat.format(DQL_HAS_OBJECT_OTHER_VERSIONS, sysObj.getChronicleId().getId(), sysObj.getObjectId().getId()));
	}

	public static boolean hasCurrentUserPermitForObject(DocbaseSession session, String objectId, int permit)
			throws DfException
	{
		String permitLevel = AclHelper.getDqlPermitLevel(permit);
		return DqlHelper.exist(session, MessageFormat.format(DQL_HAS_PERMITS_FOR_OBJECT, permitLevel, objectId));
	}

	public static void copyObjectAttributes(DocbaseSession session, IDfPersistentObject source, IDfPersistentObject target, String[] attrNames)
			throws DfException
	{
		if (source == null)
		{
			throw new IllegalArgumentException("Source object couldn't be null");
		}

		if (target == null)
		{
			throw new IllegalArgumentException("Target object couldn't be null");
		}

		if (attrNames == null || attrNames.length == 0)
		{
			return;
		}

		String sourceType = source.getType().getName();
		Map<String, DocbaseTypesHelper.AttributeInfo> sourceAttrs = DocbaseTypesHelper.getTypeAttributesInfo(session, sourceType, attrNames);

		String targetType = target.getType().getName();
		Map<String, DocbaseTypesHelper.AttributeInfo> targetAttrs = DocbaseTypesHelper.getTypeAttributesInfo(session, targetType, attrNames);

		for (String attrName : attrNames)
		{
			DocbaseTypesHelper.AttributeInfo sourceAttrInfo = sourceAttrs.get(attrName);
			if (sourceAttrInfo == null)
			{
				throw new IllegalArgumentException("Source object doesn't have '" + attrName + "' attribute");
			}

			DocbaseTypesHelper.AttributeInfo targetAttrInfo = targetAttrs.get(attrName);
			if (targetAttrInfo == null)
			{
				throw new IllegalArgumentException("Target object doesn't have '" + attrName + "' attribute");
			}

			if (sourceAttrInfo.isRepeating() != targetAttrInfo.isRepeating())
			{
				throw new IllegalArgumentException("Source and target attributes '" + attrName + "' are not equal. " +
						"One of them is repeating and one not.");
			}

			if (sourceAttrInfo.getDataType() != targetAttrInfo.getDataType())
			{
				throw new IllegalArgumentException("Source and target attributes '" + attrName + "' are not equal. " +
						"They have different data type.");
			}

			copyObjectAttribute(source, target, attrName, sourceAttrInfo.getDataType(), sourceAttrInfo.isRepeating());
		}
	}

	public static String getDocbaseOwner(DocbaseSession session)
			throws DfException
	{
		String docbaseName = session.getDocbaseName();

		synchronized (docbaseOwners)
		{
			if (docbaseOwners.containsKey(docbaseName))
			{
				return docbaseOwners.get(docbaseName);
			}

			String owner = DqlHelper.getStringParamFromFirstString(session, DQL_GET_DOCBASE_OWNER);
			docbaseOwners.put(docbaseName, owner);

			return owner;
		}
	}

	public static String getAttributeValueAsString(IDfTypedObject obj, String attrName, String dateTimeFormat)
	{
		try
		{
			int attributeType = obj.getAttrDataType(attrName);
			boolean isRepeating = obj.isAttrRepeating(attrName);

			if (attributeType == IDfType.DF_UNDEFINED)
			{
				throw new BuildException("Undefined datatype for attribute: " + attrName);
			}

			if (!isRepeating)
			{
				if (attributeType != IDfType.DF_TIME)
				{
					return obj.getString(attrName);
				}

				IDfTime time = obj.getTime(attrName);
				return dateTimeFormat == null || dateTimeFormat.length() == 0 ? time.toString() : time.asString(dateTimeFormat);
			}

			int count = obj.getValueCount(attrName);
			if (count == 0)
			{
				return null;
			}

			StringBuilder builder = new StringBuilder();
			for (int i = 0; i < count; i++)
			{
				if (i != 0)
				{
					builder.append(",");
				}

				if (attributeType != IDfType.DF_TIME)
				{
					builder.append(obj.getRepeatingString(attrName, i));
					continue;
				}

				IDfTime time = obj.getRepeatingTime(attrName, i);
				String strTime = dateTimeFormat == null || dateTimeFormat.length() == 0 ? time.toString() : time.asString(dateTimeFormat);
				builder.append(strTime);
			}

			return builder.toString();
		}
		catch (DfException e)
		{
			throw new BuildException("Failed to get datatype for attribute: " + attrName, e);
		}
	}

	private static void copyObjectAttribute(IDfPersistentObject source, IDfPersistentObject target, String attribute,
											int dataType, boolean isRepeating)
			throws DfException
	{
		if (!isRepeating)
		{
			switch (dataType)
			{
				case IDfValue.DF_BOOLEAN:
					target.setBoolean(attribute, source.getBoolean(attribute));
					return;
				case IDfValue.DF_INTEGER:
					target.setInt(attribute, source.getInt(attribute));
					return;
				case IDfValue.DF_DOUBLE:
					target.setDouble(attribute, source.getDouble(attribute));
					return;
				case IDfValue.DF_ID:
					target.setId(attribute, source.getId(attribute));
					return;
				case IDfValue.DF_STRING:
					target.setString(attribute, source.getString(attribute));
					return;
				case IDfValue.DF_TIME:
					target.setTime(attribute, source.getTime(attribute));
					return;
			}
		}

		int count = source.getValueCount(attribute);
		for (int i = 0; i < count; i++)
		{
			switch (dataType)
			{
				case IDfValue.DF_BOOLEAN:
					target.setRepeatingBoolean(attribute, i, source.getRepeatingBoolean(attribute, i));
					continue;
				case IDfValue.DF_INTEGER:
					target.setRepeatingInt(attribute, i, source.getRepeatingInt(attribute, i));
					continue;
				case IDfValue.DF_DOUBLE:
					target.setRepeatingDouble(attribute, i, source.getRepeatingDouble(attribute, i));
					continue;
				case IDfValue.DF_ID:
					target.setRepeatingId(attribute, i, source.getRepeatingId(attribute, i));
					continue;
				case IDfValue.DF_STRING:
					target.setRepeatingString(attribute, i, source.getRepeatingString(attribute, i));
					continue;
				case IDfValue.DF_TIME:
					target.setRepeatingTime(attribute, i, source.getRepeatingTime(attribute, i));
			}
		}
	}
}
