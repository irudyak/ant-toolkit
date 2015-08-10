package com.anttoolkit.documentum.common;

import com.documentum.fc.client.*;
import com.documentum.fc.common.*;

import java.text.MessageFormat;
import java.util.*;

public class DocbaseTypesHelper
{
	public static class AttributeInfo
	{
		private boolean repeating = false;
		private int dataType = IDfValue.DF_UNDEFINED;

		private AttributeInfo(boolean repeating, int dataType)
		{
			this.repeating = repeating;
			this.dataType = dataType;
		}

		public boolean isRepeating()
		{
			return repeating;
		}

		public int getDataType()
		{
			return dataType;
		}
	}

	private static final String DQL_GET_TYPE_SUPERTYPE = "select super_name from dm_type where name=''{0}''";
	private static final String DQL_GET_TYPE_OBJECT = "select r_object_id from dm_type where name=''{0}''";

	private static volatile Map<String, List<String>> typeInheritanceSequence = new HashMap<String, List<String>>();

	private static volatile Map<String, Map<String, AttributeInfo>> typeAttributes = new HashMap<String, Map<String, AttributeInfo>>();

	/**
	 * Verifies if one docbase type inherits from another type
	 * @param childTypeName child type
	 * @param parentTypeName parent type
	 * @return true if childTypeName inherits from parentTypeName
	 * @throws com.documentum.fc.common.DfException if any server error occured
	 */
	public static boolean inheritsFromType(DocbaseSession session, String childTypeName, String parentTypeName)
			throws DfException
	{
		if (ConversionHelper.isEmptyString(childTypeName) || ConversionHelper.isEmptyString(parentTypeName))
		{
			return false;
		}

		List inheritanceSequence = getTypeInheritanceSequence(session, childTypeName);

		return inheritanceSequence != null && inheritanceSequence.contains(parentTypeName);
	}

	/**
	 * Return docbase type supertypes ihreritance sequence
	 * @param typeName docbase type name
	 * @return null if docbase type doesn't inherits from any types
	 * or list containing supertype names in an order of docbase types ihreritance sequence
	 * @throws DfException if any server error occured
	 */
	public static List<String> getTypeInheritanceSequence(DocbaseSession session, String typeName)
			throws DfException
	{
		if (typeName == null)
		{
			return null;
		}

		synchronized (typeInheritanceSequence)
		{
			List<String> inheritanceSequence = typeInheritanceSequence.get(typeName);
			if (inheritanceSequence != null)
			{
				return inheritanceSequence.size() == 0 ? null : inheritanceSequence;
			}


			String superTypeName = null;
			try
			{
				superTypeName = DqlHelper.getStringParamFromFirstString(session, MessageFormat.format(DQL_GET_TYPE_SUPERTYPE, typeName));
			}
			catch (DfEndOfCollectionException e)
			{
			}

			inheritanceSequence = new Vector<String>();

			while (!ConversionHelper.isEmptyString(superTypeName))
			{
				inheritanceSequence.add(superTypeName);
				superTypeName = DqlHelper.getStringParamFromFirstString(session, MessageFormat.format(DQL_GET_TYPE_SUPERTYPE, superTypeName));
			}

			typeInheritanceSequence.put(typeName, inheritanceSequence);

			return inheritanceSequence.size() == 0 ? null : inheritanceSequence;
		}
	}

	public static Map<String, AttributeInfo> getTypeAttributesInfo(DocbaseSession session, String typeName, String... attrNames)
			throws DfException
	{
		if (ConversionHelper.isEmptyString(typeName))
		{
			throw new IllegalArgumentException("Type name couldn't be null");
		}

		if (attrNames == null || attrNames.length == 0)
		{
			throw new IllegalArgumentException("Attribute names couldn't be null");
		}

		Map<String, AttributeInfo> attrInfo;

		synchronized (typeAttributes)
		{
			attrInfo = typeAttributes.get(typeName);
			if (attrInfo == null)
			{
				attrInfo = Collections.synchronizedMap(new HashMap<String, AttributeInfo>());
				typeAttributes.put(typeName, attrInfo);
			}
		}

		boolean hasMissedAttributes = false;
		for (String attrName : attrNames)
		{
			if (!attrInfo.containsKey(attrName))
			{
				hasMissedAttributes = true;
				break;
			}
		}

		if (!hasMissedAttributes)
		{
			return attrInfo;
		}

		Map<String, AttributeInfo> loadedAttrInfo = loadTypeAttributesInfo(session, typeName, attrNames);
		for (String attrName : attrNames)
		{
			if (!attrInfo.containsKey(attrName))
			{
				attrInfo.put(attrName, loadedAttrInfo.get(attrName));
			}
		}

		return attrInfo;
	}

	private static Map<String, AttributeInfo> loadTypeAttributesInfo(DocbaseSession session, String typeName, String... attrNames)
			throws DfException
	{
		IDfType type = loadType(session, typeName);
		Map<String, AttributeInfo> attributesInfo = new HashMap<String, AttributeInfo>();

		for (String attrName : attrNames)
		{
			AttributeInfo attrInfo = null;

			try
			{
				int dataType = type.getTypeAttrDataType(attrName);
				boolean isRepeating = type.isTypeAttrRepeating(attrName);
				attrInfo = new AttributeInfo(isRepeating, dataType);
			}
			catch (DfException e)
			{
				if (DfExceptionHelper.hasMessageIdInChain(e, DfExceptionHelper.INCORRECT_OBJECT_ATTRIBUTE))
				{
					throw new DfException("Type '" + typeName + "' doesn't have attribute '" + attrName + "'", e);
				}

				throw e;
			}

			attributesInfo.put(attrName, attrInfo);
		}

		return attributesInfo;
	}

	private static IDfType loadType(DocbaseSession session, String typeName)
			throws DfException
	{
		try
		{
			IDfId id = DqlHelper.getIdParamFromFirstString(session, MessageFormat.format(DQL_GET_TYPE_OBJECT, typeName));
			return (IDfType)session.getDfObject(id);
		}
		catch (DfException e)
		{
			throw new DfException("Failed to get IDfType object for type '" + typeName + "' from docbase", e);
		}
	}
}
