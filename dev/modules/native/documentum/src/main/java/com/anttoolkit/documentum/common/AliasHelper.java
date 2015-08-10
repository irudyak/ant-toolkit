package com.anttoolkit.documentum.common;

import com.documentum.fc.common.*;
import com.documentum.fc.client.*;

import java.text.*;
import java.util.*;

public class AliasHelper
{
	private static final String FULL_ALIAS_NAME_TEMPLATE = "%{0}.{1}";

	private static final String ALIAS_NAME = "alias_name";
	private static final String ALIAS_VALUE = "alias_value";

	private static final String DQL_GET_WORKFLOW_ALIAS_SET_ID = "select s.r_object_id" +
			" from dm_alias_set s, dm_workflow w " +
			"where w.r_alias_set_id = s.r_object_id" +
			" and w.r_object_id = ''{0}''";

	private static final String DQL_ALIAS_SET_NAME_VALUE_PAIRS_BY_ALIAS_SET_NAME = "select alias_name, alias_value " +
			"from dm_alias_set where object_name = ''{0}''";

	private static final String DQL_ALIAS_SET_NAME_VALUE_PAIRS_BY_ALIAS_SET_ID = "select alias_name, alias_value " +
			"from dm_alias_set where r_object_id = ''{0}''";

	public static String getFullAliasName(String aliasSetName, String aliasName)
	{
		return MessageFormat.format(FULL_ALIAS_NAME_TEMPLATE, aliasSetName, aliasName);
	}

	public static Map<String, String> getAliasSetNameValuePairs(DocbaseSession session, String aliasSetName)
			throws DfEndOfCollectionException, DfException
	{
		IDfCollection coll = null;

		try
		{
			String dqlQuery = MessageFormat.format(DQL_ALIAS_SET_NAME_VALUE_PAIRS_BY_ALIAS_SET_NAME, aliasSetName);
			coll = DqlHelper.executeReadQuery(session, dqlQuery);

			if (coll == null || !coll.next())
			{
				throw new DfEndOfCollectionException();
			}

			Map<String, String> map = new HashMap<String, String>();
			do
			{
				IDfTypedObject obj = coll.getTypedObject();
				String name = obj.getString(AliasHelper.ALIAS_NAME);
				String value = obj.getString(AliasHelper.ALIAS_VALUE);

				map.put(name, value);
			} while (coll.next());

			return map;
		}
		finally
		{
			DqlHelper.closeCollection(coll);
		}
	}

	public static Map<String, String> getAliasSetNameValuePairs(DocbaseSession session, IDfId aliasSetId)
			throws DfEndOfCollectionException, DfException
	{
		IDfCollection coll = null;

		try
		{
			String dqlQuery = MessageFormat.format(DQL_ALIAS_SET_NAME_VALUE_PAIRS_BY_ALIAS_SET_ID, aliasSetId.toString());
			coll = DqlHelper.executeReadQuery(session, dqlQuery);

			if (coll == null || !coll.next())
			{
				throw new DfEndOfCollectionException();
			}

			Map<String, String> map = new HashMap<String, String>();
			do
			{
				IDfTypedObject obj = coll.getTypedObject();
				String name = obj.getString(AliasHelper.ALIAS_NAME);
				String value = obj.getString(AliasHelper.ALIAS_VALUE);

				map.put(name, value);
			} while (coll.next());

			return map;
		}
		finally
		{
			DqlHelper.closeCollection(coll);
		}
	}

	public static void setAliasSetNameValuePairs(DocbaseSession session, IDfId aliasSetId, Map<String, String> nameValuePairs)
			throws DfEndOfCollectionException, DfException
	{
		if (nameValuePairs == null || nameValuePairs.size() == 0 || ConversionHelper.isNullId(aliasSetId))
		{
			return;
		}

		IDfAliasSet aliasSet = (IDfAliasSet)session.getDfObject(aliasSetId);

		Set<String> names = nameValuePairs.keySet();
		for (String name : names)
		{
			int index = aliasSet.findAliasIndex(name);

			if(aliasSet.findAliasIndex(name) == -1)
			{
				aliasSet.appendAlias(name, nameValuePairs.get(name), IDfAliasSet.CATETORY_UNKNOWN, 0, "");
				continue;
			}

			aliasSet.setAliasValue(index, nameValuePairs.get(name));
		}

		aliasSet.save();
	}

	public static void setAliasSetNameValuePairs(DocbaseSession session, String aliasSetName, Map<String, String> nameValuePairs)
			throws DfEndOfCollectionException, DfException
	{
		if (nameValuePairs == null || nameValuePairs.size() == 0 || ConversionHelper.isEmptyString(aliasSetName))
		{
			return;
		}

		IDfId id = DocbaseObjectsHelper.getObjectIdByObjectName(session, "dm_alias_set", aliasSetName);

		setAliasSetNameValuePairs(session, id, nameValuePairs);
	}

	public static String getAliasValue(DocbaseSession session, String aliasSetName, String aliasName)
			throws DfEndOfCollectionException, DfException
	{
		Map table = getAliasSetNameValuePairs(session, aliasSetName);
		if (table.containsKey(aliasName))
		{
			return (String)table.get(aliasName);
		}

		throw new DfEndOfCollectionException();
	}

	public static IDfId getWorkflowAliasSetId(DocbaseSession session, IDfId workflowId)
			throws DfException, DfEndOfCollectionException
	{
		return DqlHelper.getIdParamFromFirstString(session, MessageFormat.format(DQL_GET_WORKFLOW_ALIAS_SET_ID, workflowId.toString()));
	}

	public static Map<String, String> getWorkflowAliasSetNameValuePairs(DocbaseSession session, IDfId workflowId)
			throws DfException, DfEndOfCollectionException
	{
		return getAliasSetNameValuePairs(session, getWorkflowAliasSetId(session, workflowId));
	}

	public static void setWorkflowAliasSetNameValuePairs(DocbaseSession session, IDfId workflowId, Map<String, String> nameValuePairs)
			throws DfException, DfEndOfCollectionException
	{
		if (ConversionHelper.isNullId(workflowId) || nameValuePairs == null || nameValuePairs.size() == 0)
		{
			return;
		}

		IDfId aliasSetId = getWorkflowAliasSetId(session, workflowId);

		setAliasSetNameValuePairs(session, aliasSetId, nameValuePairs);
	}
}
