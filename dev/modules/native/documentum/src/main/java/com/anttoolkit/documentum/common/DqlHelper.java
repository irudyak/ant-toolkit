package com.anttoolkit.documentum.common;

import com.documentum.fc.client.*;
import com.documentum.fc.common.*;

public class DqlHelper
{
	/**
	 * Executes read(SELECT) DQL query
	 * @param session DFC session wrapper
	 * @param dqlStatement statement
	 * @return collection
	 * @throws com.documentum.fc.common.DfException if any server errors occured
	 */
	public static IDfCollection executeReadQuery(DocbaseSession session, String dqlStatement)
			throws DfException
	{
		return executeDfQuery(session, dqlStatement, IDfQuery.DF_EXECREAD_QUERY);
	}

	/**
	 * The same as "executeQuery" function but doesn't return any results.
	 * Use this method only for delete, update and insert queries
	 * @param session DFC session wrapper
	 * @param dqlStatement statement
	 * @throws DfException if any server errors occured
	 */
	public static void executeQuery(DocbaseSession session, String dqlStatement)
			throws DfException
	{
		IDfCollection coll = null;

		try
		{
			coll = executeDfQuery(session, dqlStatement, IDfQuery.EXEC_QUERY);
		}
		finally
		{
			closeCollection(coll);
		}
	}

	/**
	 * Verifies if query returns any results
	 * @param session DFC session wrapper
	 * @param dqlStatement statement
	 * @return true if any results exist otherwise false
	 * @throws DfException if any server error occured
	 */
	public static boolean exist(DocbaseSession session, String dqlStatement)
			throws DfException
	{
		IDfCollection coll = null;
		try
		{
			coll = executeReadQuery(session, dqlStatement);
			return coll != null && coll.next();
		}
		finally
		{
			closeCollection(coll);
		}
	}

	/**
	 * Closes opened collection
	 * @param coll collection
	 */
	public static void closeCollection(IDfCollection coll)
	{
		try
		{
			if (coll != null &&
				(coll.getState() == IDfCollection.DF_READY_STATE ||
				coll.getState() == IDfCollection.DF_NO_MORE_ROWS_STATE))
			{
				coll.close();
			}
		}
		catch (DfException e)
		{
			System.out.println("DqlHelper:closeCollection Failed to close collection\r\n\r\n" + e.toString());
		}
	}

	/**
	 * Retrieves first string from DQL query
	 * @param session DFC session wrapper
	 * @param dqlStatement DQL statement
	 * @return typed object
	 * @throws com.documentum.fc.common.DfEndOfCollectionException if collection is empty
	 * @throws DfException if any server errors occured
	 */
	public static IDfTypedObject getFirstString(DocbaseSession session, String dqlStatement)
			throws DfEndOfCollectionException, DfException
	{
		IDfCollection coll = null;

		try
		{
			coll = executeReadQuery(session, dqlStatement);
			if (coll == null || !coll.next())
			{
				throw new DfEndOfCollectionException();
			}

			return coll.getTypedObject();
		}
		finally
		{
			closeCollection(coll);
		}
	}

	/**
	 * Executes DQL query, retrieves parameter from the first record in collection and closes it
	 * @param session DFC session wrapper
	 * @param dqlStatement DQL statement
	 * @param parameterName parameter name
	 * @return parameter value
	 * @throws DfEndOfCollectionException if collection is empty
	 * @throws DfException if any server errors occured
	 */
	public static IDfValue getParamFromFirstString(DocbaseSession session, String dqlStatement, String parameterName)
			throws DfEndOfCollectionException, DfException
	{
		IDfCollection coll = null;

		try
		{
			coll = executeReadQuery(session, dqlStatement);
			if (coll == null || !coll.next())
			{
				throw new DfEndOfCollectionException();
			}

			return coll.getTypedObject().getValue(parameterName);
		}
		finally
		{
			closeCollection(coll);
		}
	}

	/**
	 * Executes DQL query, retrieves parameter from the first record in collection and closes it
	 * @param session DFC session wrapper
	 * @param dqlStatement DQL statement
	 * @param parameterPosition parameter position in the record
	 * @return parameter value
	 * @throws DfEndOfCollectionException if collection is empty
	 * @throws DfException if any server errors occured
	 */
	public static IDfValue getParamFromFirstString(DocbaseSession session, String dqlStatement, int parameterPosition)
			throws DfEndOfCollectionException, DfException
	{
		IDfCollection coll = null;

		try
		{
			coll = executeReadQuery(session, dqlStatement);
			if (coll == null || !coll.next())
			{
				throw new DfEndOfCollectionException();
			}

			return coll.getTypedObject().getValueAt(parameterPosition);
		}
		finally
		{
			closeCollection(coll);
		}
	}

	/**
	 * Executes DQL query, retrieves first parameter from the first record in collection and closes it
	 * @param session DFC session wrapper
	 * @param dqlStatement DQL statement
	 * @return parameter value
	 * @throws DfEndOfCollectionException if collection is empty
	 * @throws DfException if any server errors occured
	 */
	public static IDfValue getParamFromFirstString(DocbaseSession session, String dqlStatement)
			throws DfEndOfCollectionException, DfException
	{
		return getParamFromFirstString(session, dqlStatement, 0);
	}

	/**
	 * Executes DQL query, retrieves String parameter from the first record in collection and closes it
	 * @param session DFC session wrapper
	 * @param dqlStatement DQL statement
	 * @param parameterName parameter name
	 * @return parameter value
	 * @throws DfEndOfCollectionException if collection is empty
	 * @throws DfException if any server errors occured
	 */
	public static String getStringParamFromFirstString(DocbaseSession session, String dqlStatement, String parameterName)
			throws DfEndOfCollectionException, DfException
	{
		return getParamFromFirstString(session, dqlStatement, parameterName).asString();
	}

	/**
	 * Executes DQL query, retrieves String parameter from the first record in collection and closes it
	 * @param session DFC session wrapper
	 * @param dqlStatement DQL statement
	 * @param parameterPosition parameter position in the record
	 * @return parameter value
	 * @throws DfEndOfCollectionException if collection is empty
	 * @throws DfException if any server errors occured
	 */
	public static String getStringParamFromFirstString(DocbaseSession session, String dqlStatement, int parameterPosition)
			throws DfEndOfCollectionException, DfException
	{
		return getParamFromFirstString(session, dqlStatement, parameterPosition).asString();
	}

	/**
	 * Executes DQL query, retrieves first parameter as "String" from the first record in collection and closes it
	 * @param session DFC session wrapper
	 * @param dqlStatement DQL statement
	 * @return parameter value
	 * @throws DfEndOfCollectionException if collection is empty
	 * @throws DfException if any server errors occured
	 */
	public static String getStringParamFromFirstString(DocbaseSession session, String dqlStatement)
			throws DfEndOfCollectionException, DfException
	{
		return getParamFromFirstString(session, dqlStatement, 0).asString();
	}

	static String getStringParamFromFirstString(IDfSession session, String dqlStatement)
			throws DfEndOfCollectionException, DfException
	{
		return getParamFromFirstString(session, dqlStatement, 0).asString();
	}

	/**
	 * Executes DQL query, retrieves int parameter from the first record in collection and closes it
	 * @param session DFC session wrapper
	 * @param dqlStatement DQL statement
	 * @param parameterName parameter name
	 * @return parameter value
	 * @throws DfEndOfCollectionException if collection is empty
	 * @throws DfException if any server errors occured
	 */
	public static int getIntegerParamFromFirstString(DocbaseSession session, String dqlStatement, String parameterName)
			throws DfEndOfCollectionException, DfException
	{
		return getParamFromFirstString(session, dqlStatement, parameterName).asInteger();
	}

	/**
	 * Executes DQL query, retrieves int parameter from the first record in collection and closes it
	 * @param session DFC session wrapper
	 * @param dqlStatement DQL statement
	 * @param parameterPosition parameter position in the record
	 * @return parameter value
	 * @throws DfEndOfCollectionException if collection is empty
	 * @throws DfException if any server errors occured
	 */
	public static int getIntegerParamFromFirstString(DocbaseSession session, String dqlStatement, int parameterPosition)
			throws DfEndOfCollectionException, DfException
	{
		return getParamFromFirstString(session, dqlStatement, parameterPosition).asInteger();
	}

	/**
	 * Executes DQL query, retrieves first parameter as "int" from the first record in collection and closes it
	 * @param session DFC session wrapper
	 * @param dqlStatement DQL statement
	 * @return parameter value
	 * @throws DfEndOfCollectionException if collection is empty
	 * @throws DfException if any server errors occured
	 */
	public static int getIntegerParamFromFirstString(DocbaseSession session, String dqlStatement)
			throws DfEndOfCollectionException, DfException
	{
		return getParamFromFirstString(session, dqlStatement, 0).asInteger();
	}

	/**
	 * Executes DQL query, retrieves double parameter from the first record in collection and closes it
	 * @param session DFC session wrapper
	 * @param dqlStatement DQL statement
	 * @param parameterName parameter name
	 * @return parameter value
	 * @throws DfEndOfCollectionException if collection is empty
	 * @throws DfException if any server errors occured
	 */
	public static double getDoubleParamFromFirstString(DocbaseSession session, String dqlStatement, String parameterName)
			throws DfEndOfCollectionException, DfException
	{
		return getParamFromFirstString(session, dqlStatement, parameterName).asDouble();
	}

	/**
	 * Executes DQL query, retrieves double parameter from the first record in collection and closes it
	 * @param session DFC session wrapper
	 * @param dqlStatement DQL statement
	 * @param parameterPosition parameter position in the record
	 * @return parameter value
	 * @throws DfEndOfCollectionException if collection is empty
	 * @throws DfException if any server errors occured
	 */
	public static double getDoubleParamFromFirstString(DocbaseSession session, String dqlStatement, int parameterPosition)
			throws DfEndOfCollectionException, DfException
	{
		return getParamFromFirstString(session, dqlStatement, parameterPosition).asDouble();
	}

	/**
	 * Executes DQL query, retrieves first parameter as "double" from the first record in collection and closes it
	 * @param session DFC session wrapper
	 * @param dqlStatement DQL statement
	 * @return parameter value
	 * @throws DfEndOfCollectionException if collection is empty
	 * @throws DfException if any server errors occured
	 */
	public static double getDoubleParamFromFirstString(DocbaseSession session, String dqlStatement)
			throws DfEndOfCollectionException, DfException
	{
		return getParamFromFirstString(session, dqlStatement, 0).asDouble();
	}

	/**
	 * Executes DQL query, retrieves boolean parameter from the first record in collection and closes it
	 * @param session DFC session wrapper
	 * @param dqlStatement DQL statement
	 * @param parameterName parameter name
	 * @return parameter value
	 * @throws DfEndOfCollectionException if collection is empty
	 * @throws DfException if any server errors occured
	 */
	public static boolean getBooleanParamFromFirstString(DocbaseSession session, String dqlStatement, String parameterName)
			throws DfEndOfCollectionException, DfException
	{
		return getParamFromFirstString(session, dqlStatement, parameterName).asBoolean();
	}

	/**
	 * Executes DQL query, retrieves boolean parameter from the first record in collection and closes it
	 * @param session DFC session wrapper
	 * @param dqlStatement DQL statement
	 * @param parameterPosition parameter position in the record
	 * @return parameter value
	 * @throws DfEndOfCollectionException if collection is empty
	 * @throws DfException if any server errors occured
	 */
	public static boolean getBooleanParamFromFirstString(DocbaseSession session, String dqlStatement, int parameterPosition)
			throws DfEndOfCollectionException, DfException
	{
		return getParamFromFirstString(session, dqlStatement, parameterPosition).asBoolean();
	}

	/**
	 * Executes DQL query, retrieves first parameter as "boolean" from the first record in collection and closes it
	 * @param session DFC session wrapper
	 * @param dqlStatement DQL statement
	 * @return parameter value
	 * @throws DfEndOfCollectionException if collection is empty
	 * @throws DfException if any server errors occured
	 */
	public static boolean getBooleanParamFromFirstString(DocbaseSession session, String dqlStatement)
			throws DfEndOfCollectionException, DfException
	{
		return getParamFromFirstString(session, dqlStatement, 0).asBoolean();
	}

	/**
	 * Executes DQL query, retrieves IDfId parameter from the first record in collection and closes it
	 * @param session DFC session wrapper
	 * @param dqlStatement DQL statement
	 * @param parameterName parameter name
	 * @return parameter value
	 * @throws DfEndOfCollectionException if collection is empty
	 * @throws DfException if any server errors occured
	 */
	public static IDfId getIdParamFromFirstString(DocbaseSession session, String dqlStatement, String parameterName)
			throws DfEndOfCollectionException, DfException
	{
		return getParamFromFirstString(session, dqlStatement, parameterName).asId();
	}

	/**
	 * Executes DQL query, retrieves IDfId parameter from the first record in collection and closes it
	 * @param session DFC session wrapper
	 * @param dqlStatement DQL statement
	 * @param parameterPosition parameter position in the record
	 * @return parameter value
	 * @throws DfEndOfCollectionException if collection is empty
	 * @throws DfException if any server errors occured
	 */
	public static IDfId getIdParamFromFirstString(DocbaseSession session, String dqlStatement, int parameterPosition)
			throws DfEndOfCollectionException, DfException
	{
		return getParamFromFirstString(session, dqlStatement, parameterPosition).asId();
	}

	/**
	 * Executes DQL query, retrieves first parameter as "IDfId" from the first record in collection and closes it
	 * @param session DFC session wrapper
	 * @param dqlStatement DQL statement
	 * @return parameter value
	 * @throws DfEndOfCollectionException if collection is empty
	 * @throws DfException if any server errors occured
	 */
	public static IDfId getIdParamFromFirstString(DocbaseSession session, String dqlStatement)
			throws DfEndOfCollectionException, DfException
	{
		return getParamFromFirstString(session, dqlStatement, 0).asId();
	}

	/**
	 * Executes DQL query, retrieves IDfTime parameter from the first record in collection and closes it
	 * @param session DFC session wrapper
	 * @param dqlStatement DQL statement
	 * @param parameterName parameter name
	 * @return parameter value
	 * @throws DfEndOfCollectionException if collection is empty
	 * @throws DfException if any server errors occured
	 */
	public static IDfTime getTimeParamFromFirstString(DocbaseSession session, String dqlStatement, String parameterName)
			throws DfEndOfCollectionException, DfException
	{
		return getParamFromFirstString(session, dqlStatement, parameterName).asTime();
	}

	/**
	 * Executes DQL query, retrieves IDfTime parameter from the first record in collection and closes it
	 * @param session DFC session wrapper
	 * @param dqlStatement DQL statement
	 * @param parameterPosition parameter position in the record
	 * @return parameter value
	 * @throws DfEndOfCollectionException if collection is empty
	 * @throws DfException if any server errors occured
	 */
	public static IDfTime getTimeParamFromFirstString(DocbaseSession session, String dqlStatement, int parameterPosition)
			throws DfEndOfCollectionException, DfException
	{
		return getParamFromFirstString(session, dqlStatement, parameterPosition).asTime();
	}

	/**
	 * Executes DQL query, retrieves first parameter as "IDfTime" from the first record in collection and closes it
	 * @param session DFC session wrapper
	 * @param dqlStatement DQL statement
	 * @return parameter value
	 * @throws DfEndOfCollectionException if collection is empty
	 * @throws DfException if any server errors occured
	 */
	public static IDfTime getTimeParamFromFirstString(DocbaseSession session, String dqlStatement)
			throws DfEndOfCollectionException, DfException
	{
		return getParamFromFirstString(session, dqlStatement, 0).asTime();
	}


//// PRIVATE MEMBERS ////

	static IDfCollection executeReadQuery(IDfSession session, String dqlStatement)
			throws DfException
	{
		return executeDfQuery(session, dqlStatement, IDfQuery.DF_EXECREAD_QUERY);
	}

	static IDfValue getParamFromFirstString(IDfSession session, String dqlStatement, int parameterPosition)
			throws DfEndOfCollectionException, DfException
	{
		IDfCollection coll = null;

		try
		{
			coll = executeReadQuery(session, dqlStatement);
			if (coll == null || !coll.next())
			{
				throw new DfEndOfCollectionException();
			}

			return coll.getTypedObject().getValueAt(parameterPosition);
		}
		finally
		{
			closeCollection(coll);
		}
	}

	static IDfValue getParamFromFirstString(IDfSession session, String dqlStatement)
			throws DfEndOfCollectionException, DfException
	{
		return getParamFromFirstString(session, dqlStatement, 0);
	}

	static IDfCollection executeDfQuery(DocbaseSession session,
												String dqlStatement,
												int queryType)
			throws DfException
	{
		IDfQuery query = new DfQuery();
		query.setDQL(removeFormattingCharacters(dqlStatement));
		return query.execute(session.getDfSession(), queryType);
	}

	static IDfCollection executeDfQuery(IDfSession session,
												String dqlStatement,
												int queryType)
			throws DfException
	{
		IDfQuery query = new DfQuery();
		query.setDQL(removeFormattingCharacters(dqlStatement));
		return query.execute(session, queryType);
	}

	private static String removeFormattingCharacters(String text)
	{
		if (text == null)
		{
			return null;
		}

		String temp = text.replace("\r", " ");
		temp = temp.replace("\n", " ");
		temp = temp.replace("\t", " ");

		return temp.trim();
	}
}
