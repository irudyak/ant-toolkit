package com.anttoolkit.sql.tasks.build.util;

import java.sql.*;
import java.util.*;

import org.apache.tools.ant.*;

import com.anttoolkit.general.tasks.build.util.*;
import com.anttoolkit.general.tasks.*;
import com.anttoolkit.general.common.*;
import com.anttoolkit.sql.common.*;

public abstract class GenericSqlVersionProvider
		extends GenericVersionProvider
{
	public static final String DRIVER_PARAM = "db.driver";

	public static class VersionRecordNotExistException extends Exception
	{
	}

	private class SqlSessionsCleaner extends Thread
	{
		@Override
		public void run()
		{
			if (GenericSqlVersionProvider.this.session != null)
			{
				GenericSqlVersionProvider.this.session.closeConnection();
			}
		}
	}

	//table and index names
	@VersionProviderParam("version.table")
	protected String versionTable = "BUILD_VERSION";

	@VersionProviderParam("history.table")
	protected String historyTable = "BUILD_HISTORY";

	@VersionProviderParam("index.name")
	protected String indexName = "BUILD_HISTORY_INDEX";

	//common attributes
	@VersionProviderParam("major.column")
	protected String majorCol = "MAJOR";

	@VersionProviderParam("minor.column")
	protected String minorCol = "MINOR";

	@VersionProviderParam("patch.column")
	protected String patchCol = "PATCH";

	@VersionProviderParam("update.time.column")
	protected String updateTimeCol = "UPDATE_TIME";

	@VersionProviderParam("qualifier.column")
	protected String qualifierCol = "QUALIFIER";

	@VersionProviderParam("dbuser.column")
	protected String dbUserCol = "DB_USER";

	@VersionProviderParam("osuser.column")
	protected String osUserCol = "OS_USER";

	@VersionProviderParam("hostname.column")
	protected String hostnameCol = "HOSTNAME";

	@VersionProviderParam("ip.column")
	protected String ipCol = "IP";

	//history attributes
	@VersionProviderParam("prev.major.column")
	protected String prevMajorCol = "MAJOR_PREV";

	@VersionProviderParam("prev.minor.column")
	protected String prevMinorCol = "MINOR_PREV";

	@VersionProviderParam("prev.patch.column")
	protected String prevPatchCol = "PATCH_PREV";

	@VersionProviderParam("comment.column")
	protected String commentCol = "COMMENT";

	//connection settings
	@VersionProviderParam("db.builds.schema")
	private String dbSchema;

	@VersionProviderParam("db.user")
	private String dbUser;

	@VersionProviderParam("db.password")
	private String dbPassword;

	@VersionProviderParam("db.driver")
	private String dbDriver;

	@VersionProviderParam("db.connection.url")
	private String dbUrl;

	private boolean checkTablesExist = true;

	private SqlSession session;

	@Override
	public void init(GenericTask task, Map<String, String> properties)
	{
		super.init(task, properties);

		//Either all params should be null or all of them should be specified
		if (dbUser == null && dbPassword == null && dbDriver == null && dbUrl == null)
		{
			return;
		}

		if (dbUser == null)
		{
			throw new BuildException("Incorrect SQL build version provider specification - user name should be specified");
		}

		if (dbPassword == null)
		{
			throw new BuildException("Incorrect SQL build version provider specification - user password should be specified");
		}

		if (dbDriver == null)
		{
			throw new BuildException("Incorrect SQL build version provider specification - database driver should be specified");
		}

		if (dbUrl == null)
		{
			throw new BuildException("Incorrect SQL build version provider specification - database connection url should be specified");
		}

		try
		{
			session = SqlSessionManager.newSession(new LoginInfo(dbUrl, dbUser, dbPassword, (Driver)ReflectionHelper.newInstance(dbDriver), null));
		}
		catch (Exception e)
		{
			throw new BuildException("Failed to create instance of SQL drive of class '" + dbDriver + "'");
		}

		Runtime.getRuntime().addShutdownHook(new SqlSessionsCleaner());
	}

	@Override
	public boolean canUpdateToVersion(BuildVersion version, String qualifier)
	{
		//workaround for the situation when version tables or records are not already created
		if (checkTablesExist)
		{
			getCurrentVersion(qualifier);
			checkTablesExist = false;
		}

		return !isVersionHistoryRecordExist(version, qualifier);
	}

	@Override
	public void updateToVersion(BuildVersion currentVersion, BuildVersion newVersion, String comment, String qualifier)
	{
		//workaround for the situation when version tables or records is not already created
		if (checkTablesExist)
		{
			getCurrentVersion(qualifier);
			checkTablesExist = false;
		}

		comment = comment == null ? "" : comment.trim();
		comment = comment.length() > 512 ? comment.substring(0, 512) : comment;

		boolean outOfTurnUpdate = newVersion.compareTo(currentVersion) < 0;

		if (!outOfTurnUpdate)
		{
			updateVersionRecord(currentVersion, newVersion, comment, qualifier);
		}

		createHistoryRecord(currentVersion, newVersion, comment, qualifier);
	}

	@Override
	public BuildVersion getCurrentVersion(String qualifier)
	{
		ResultSet resultSet = null;

		try
		{
			String query = getSqlToCheckVersion(qualifier);

			resultSet = SqlHelper.executeQuery(getSession(), query);
			if (!resultSet.next())
			{
				throw new VersionRecordNotExistException();
			}

			int major = Integer.parseInt(SqlHelper.getColumnValue(resultSet, majorCol));
			int minor = Integer.parseInt(SqlHelper.getColumnValue(resultSet, minorCol));
			int patch = Integer.parseInt(SqlHelper.getColumnValue(resultSet, patchCol));

			return new BuildVersion(major, minor, patch);
		}
		catch (Throwable e)
		{
			return handleGetVersionException(e, qualifier);
		}
		finally
		{
			SqlHelper.closeResultSet(resultSet);
		}
	}

	public String getDbSchema()
	{
		return dbSchema != null && !dbSchema.trim().isEmpty() ? dbSchema + "." : "";
	}

	protected abstract String getSqlToCheckCanUpdateToVersion(BuildVersion buildVersion, String qualifier);

	protected abstract String getSqlToCheckVersionHistory(BuildVersion buildVersion, String qualifier);

	protected abstract String getSqlToCheckVersion(String qualifier);

	protected abstract String getSqlToCheckVersionTable();

	protected abstract String getSqlToCheckHistoryTable();

	protected abstract String getSqlToUpdateVersionRecord(BuildVersion currentVersion, BuildVersion newVersion, String comment, String qualifier);

	protected abstract String getSqlToCreateHistoryRecord(BuildVersion currentVersion, BuildVersion newVersion, String comment, String qualifier);

	protected abstract String getSqlToCreateVersionRecord(String qualifier);

	protected abstract String getSqlToCreateVersionTable();

	protected abstract String getSqlToCreateHistoryTable();

	protected abstract boolean isVersionRecordNotExistException(Throwable e);

	protected abstract boolean isTableNotExistException(Throwable e);

	protected boolean isVersionHistoryRecordExist(BuildVersion buildVersion, String qualifier)
	{
		try
		{
			String query = getSqlToCheckVersionHistory(buildVersion, qualifier);
			return SqlHelper.exist(getSession(), query, (String)null);
		}
		catch (Throwable e)
		{
			return handleGetVersionUpdateHistoryException(e);
		}
	}

	protected void updateVersionRecord(BuildVersion currentVersion, BuildVersion newVersion, String comment, String qualifier)
	{
		String query = getSqlToUpdateVersionRecord(currentVersion, newVersion, comment, qualifier);

		try
		{
			SqlHelper.executeUpdate(getSession(), query);
		}
		catch (Throwable e)
		{
			throw new BuildException("Failed to update database schema version to " +
					newVersion.toString() +
					qualifier == null || qualifier.trim().isEmpty() ? "" : " [" + qualifier + "]", e);
		}
	}

	protected BuildVersion handleGetVersionException(Throwable e, String qualifier)
	{
		if (isVersionRecordNotExistException(e))
		{
			createVersionRecord(qualifier);
			return BuildVersion.INITIAL;
		}

		if (isTableNotExistException(e))
		{
			createVersionTable();
			createVersionRecord(qualifier);
			return BuildVersion.INITIAL;
		}

		throw new BuildException("Error occured while trying to get current build version", e);
	}

	protected boolean handleGetVersionUpdateHistoryException(Throwable e)
	{
		if (isTableNotExistException(e))
		{
			createHistoryTable();
			return false;
		}

		throw new BuildException("Failed to check version history", e);
	}

	protected void createVersionRecord(String qualifier)
	{
		String query = getSqlToCreateVersionRecord(qualifier);

		try
		{
			SqlHelper.executeUpdate(getSession(), query);
		}
		catch (Throwable e)
		{
			throw new BuildException("Failed to create database schema version record, qualifier=" + qualifier, e);
		}
	}

	protected void createHistoryRecord(BuildVersion currentVersion, BuildVersion newVersion, String comment, String qualifier)
	{
		String query = getSqlToCreateHistoryRecord(currentVersion, newVersion, comment, qualifier);

		try
		{
			SqlHelper.executeUpdate(getSession(), query);
		}
		catch (Throwable e)
		{
			throw new BuildException("Failed to create database schema version history for " +
					currentVersion.toString() + "->"  + newVersion.toString() +
					qualifier == null || qualifier.trim().isEmpty() ? "" : " [" + qualifier + "]", e);
		}
	}

	protected void createVersionTable()
	{
		try
		{
			SqlHelper.executeUpdate(getSession(), getSqlToCreateVersionTable());
		}
		catch (Throwable e)
		{
			throw new BuildException("Failed to create version table", e);
		}
	}

	protected void createHistoryTable()
	{
		try
		{
			SqlHelper.executeUpdate(getSession(), getSqlToCreateHistoryTable());
		}
		catch (Throwable e)
		{
			throw new BuildException("Failed to create history table", e);
		}
	}

	@Override
	protected void finalize() throws Throwable
	{
		super.finalize();

		if (session != null)
		{
			session.closeConnection();
		}
	}

	private SqlSession getSession()
	{
		return session != null ? session : SqlSessionManager.getSession();
	}
}
