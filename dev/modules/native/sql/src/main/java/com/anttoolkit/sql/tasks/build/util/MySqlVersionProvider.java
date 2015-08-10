package com.anttoolkit.sql.tasks.build.util;

import java.sql.*;

import org.apache.tools.ant.*;

import com.anttoolkit.general.common.*;
import com.anttoolkit.sql.common.*;
import com.anttoolkit.general.tasks.build.util.*;

public class MySqlVersionProvider
		extends GenericSqlVersionProvider
{
	protected void createHistoryTable()
	{
		super.createHistoryTable();

		final String sqlIndex1 = "CREATE INDEX " + getDbSchema() + indexName + "_1 " +
				"ON " + getDbSchema() + historyTable + " (" + majorCol + ", " + minorCol + ", " + patchCol + ")";

		final String sqlIndex2 = "CREATE INDEX " + getDbSchema() + indexName + "_2 " +
				"ON " + getDbSchema() + historyTable + " (" + prevMajorCol + ", " + prevMinorCol + ", " + prevPatchCol + ")";

		//creating indexes
		try
		{

			SqlHelper.executeUpdate(SqlSessionManager.getSession(), sqlIndex1);
			SqlHelper.executeUpdate(SqlSessionManager.getSession(), sqlIndex2);
		}
		catch (Throwable e)
		{
			throw new BuildException("Failed to create history table", e);
		}
	}

	@Override
	protected String getSqlToCheckCanUpdateToVersion(BuildVersion buildVersion, String qualifier)
	{
		return "SELECT * FROM " + getDbSchema() + versionTable +
				" WHERE (" + majorCol + " < " + buildVersion.major + " OR (" + majorCol + "=" + buildVersion.major +
				" AND " + minorCol + " < " + buildVersion.minor + ") OR (" + majorCol + "=" + buildVersion.major +
				" AND " + minorCol + "=" + buildVersion.minor + " AND " + patchCol + " < " + buildVersion.patch + "))" +
				" AND " + qualifierCol + getQualifierWhereCondition(qualifier);
	}

	@Override
	protected String getSqlToCheckVersionHistory(BuildVersion buildVersion, String qualifier)
	{
		return "SELECT * FROM " + getDbSchema() + historyTable +
				" WHERE " + majorCol + "=" + buildVersion.major + " AND " +
				minorCol + "=" + buildVersion.minor + " AND " +
				patchCol + "=" + buildVersion.patch + " AND " +
				qualifierCol + getQualifierWhereCondition(qualifier);
	}

	protected String getSqlToCheckVersion(String qualifier)
	{
		return "SELECT " + majorCol + ", " + minorCol + ", " + patchCol +
				" FROM " + getDbSchema() + versionTable + " WHERE " + qualifierCol + getQualifierWhereCondition(qualifier);
	}

	@Override
	protected String getSqlToCheckVersionTable()
	{
		return "SELECT " + majorCol + ", " + minorCol + ", " + patchCol + ", " + qualifierCol + ", " +
				commentCol + ", " + updateTimeCol + ", " + dbUserCol + ", " + osUserCol + ", " + hostnameCol + ", " + ipCol +
				" FROM " + getDbSchema() + versionTable + " LIMIT 1";
	}

	@Override
	protected String getSqlToCheckHistoryTable()
	{
		return "SELECT " + majorCol + ", " + minorCol + ", " +
				patchCol + ", " + qualifierCol + ", " + updateTimeCol + ", " + dbUserCol + ", " +
				osUserCol + ", " + hostnameCol + ", " + ipCol + ", " + prevMajorCol + ", " +
				prevMinorCol + ", " + prevPatchCol + ", " + commentCol +
				" FROM " + getDbSchema() + historyTable + " LIMIT 1";
	}

	@Override
	protected String getSqlToUpdateVersionRecord(BuildVersion currentVersion, BuildVersion newVersion, String comment, String qualifier)
	{
		return "UPDATE " + getDbSchema() + versionTable +
				" set " + majorCol + "=" + newVersion.major + ", " +
				minorCol + "=" + newVersion.minor + ", " +
				patchCol + "=" + newVersion.patch + ", " +
				commentCol + "='" + comment + "', " +
				updateTimeCol + "=SYSDATE(), " +
				dbUserCol + "=CURRENT_USER(), " +
				osUserCol + "='" + SystemHelper.osUser + "', " +
				hostnameCol + "='" + SystemHelper.hostName + "', " +
				ipCol + "='" + SystemHelper.hostIp + "' " +
				"WHERE " + majorCol + "=" + currentVersion.major + " AND " +
				minorCol + "=" + currentVersion.minor + " AND " +
				patchCol + "=" + currentVersion.patch + " AND " +
				qualifierCol + getQualifierWhereCondition(qualifier);
	}

	@Override
	protected String getSqlToCreateHistoryRecord(BuildVersion currentVersion, BuildVersion newVersion, String comment, String qualifier)
	{
		return "INSERT INTO " + getDbSchema() + historyTable +
				"(" + majorCol + ", " + minorCol + ", " + patchCol + ", " +
				prevMajorCol + ", " + prevMinorCol + ", " + prevPatchCol + ", " +
				commentCol + ", " + qualifierCol + ", " + updateTimeCol + ", " + dbUserCol + ", " +
				osUserCol + ", " + hostnameCol + ", " + ipCol + ") " +
				"VALUES (" + newVersion.major + ", " + newVersion.minor + ", " + newVersion.patch + ", " +
				currentVersion.major + ", " + currentVersion.minor + ", " + currentVersion.patch + ", '" +
				comment + "', '" + qualifier + "', SYSDATE(), CURRENT_USER(), '" + SystemHelper.osUser + "', '" +
				SystemHelper.hostName + "', '" + SystemHelper.hostIp + "')";
	}

	@Override
	protected String getSqlToCreateVersionRecord(String qualifier)
	{
		return "INSERT INTO " + getDbSchema() + versionTable +
				"(" + majorCol + ", " + minorCol + ", " + patchCol + ", " +
				qualifierCol + ", " + updateTimeCol + ", " + dbUserCol + ", " +
				osUserCol + ", " + hostnameCol + ", " + ipCol + ") " +
				"VALUES (0, 0, 0, '" + qualifier + "', SYSDATE(), CURRENT_USER(), '" +
				SystemHelper.osUser + "', '" + SystemHelper.hostName + "', '" + SystemHelper.hostIp + "')";
	}

	@Override
	protected String getSqlToCreateVersionTable()
	{
		return "CREATE TABLE " + getDbSchema() + versionTable + " " +
				"(" + majorCol + " INT, " + minorCol + " INT, " + patchCol + " INT, " +
				commentCol + " VARCHAR(512), " +  qualifierCol + " VARCHAR(64), " +
				updateTimeCol + " DATETIME, " +  dbUserCol + " VARCHAR(64), " + osUserCol + " VARCHAR(64), " +
				hostnameCol + " VARCHAR(64), " + ipCol + " VARCHAR(64))";
	}

	@Override
	protected String getSqlToCreateHistoryTable()
	{
		return "CREATE TABLE " + getDbSchema() + historyTable + " " +
				"(" + majorCol + " INT, " + minorCol + " INT, " + patchCol + " INT, " +
				prevMajorCol + " INT, " + prevMinorCol + " INT, " + prevPatchCol + " INT, " +
				commentCol + " VARCHAR(512), " + qualifierCol + " VARCHAR(64), " +
				updateTimeCol + " DATETIME, " + dbUserCol + " VARCHAR(64), " +
				osUserCol + " VARCHAR(64), " + hostnameCol + " VARCHAR(64), " +
				ipCol + " VARCHAR(64))";
	}

	@Override
	protected boolean isVersionRecordNotExistException(Throwable e)
	{
		return e instanceof VersionRecordNotExistException;
	}

	@Override
	protected boolean isTableNotExistException(Throwable e)
	{
		Throwable ex = e instanceof BuildException ? e.getCause() : e;
		return ex instanceof SQLException && ((SQLException)ex).getErrorCode() == 1146;
	}

	protected String getQualifierWhereCondition(String qualifier)
	{
		return qualifier == null || qualifier.trim().length() == 0 ?
				"=''" : "='" + qualifier + "'";
	}
}
