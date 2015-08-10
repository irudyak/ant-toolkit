package com.anttoolkit.documentum.tasks.build.util;

import java.util.*;

import org.apache.tools.ant.*;

import com.documentum.fc.client.*;
import com.documentum.fc.common.*;

import com.anttoolkit.documentum.common.*;
import com.anttoolkit.general.common.*;
import com.anttoolkit.general.tasks.build.util.*;
import com.anttoolkit.general.tasks.*;

public class DocbaseVersionProvider
		extends GenericVersionProvider
{
	private class DocbaseSessionsCleaner extends Thread
	{
		@Override
		public void run()
		{
			if (DocbaseVersionProvider.this.session != null)
			{
				DocbaseVersionProvider.this.session.releaseSession();
			}
		}
	}

	private static final String TYPE_NOT_EXIST_ERROR = "DM_QUERY_E_REG_TABLE_QUAL";
	private static final String TABLE_NOT_FOUND_ERROR = "DM_QUERY2_E_TABLE_NOT_FOUND";
	private static final String CURSOR_ERROR = "DM_QUERY_E_CURSOR_ERROR";

	//tables
	@VersionProviderParam("version.table")
	protected String versionTable = "build_version";

	@VersionProviderParam("history.table")
	protected String historyTable = "build_history";

	//common attributes
	@VersionProviderParam("major.column")
	protected String majorCol = "major";

	@VersionProviderParam("minor.column")
	protected String minorCol = "minor";

	@VersionProviderParam("patch.column")
	protected String patchCol = "patch";

	@VersionProviderParam("update.time.column")
	protected String updateTimeCol = "update_time";

	@VersionProviderParam("qualifier.column")
	protected String qualifierCol = "qualifier";

	@VersionProviderParam("docbase.user.column")
	protected String docbaseUserCol = "docbase_user";

	@VersionProviderParam("osuser.column")
	protected String osUserCol = "os_user";

	@VersionProviderParam("hostname.column")
	protected String hostnameCol = "hostname";

	@VersionProviderParam("ip.column")
	protected String ipCol = "ip";

	//history attributes
	@VersionProviderParam("prev.major.column")
	protected String prevMajorCol = "major_prev";

	@VersionProviderParam("prev.minor.column")
	protected String prevMinorCol = "minor_prev";

	@VersionProviderParam("prev.patch.column")
	protected String prevPatchCol = "patch_prev";

	@VersionProviderParam("comment.column")
	protected String commentCol = "build_comment";

	//connection settings
	@VersionProviderParam("docbase.name")
	private String docbaseName;

	@VersionProviderParam("docbase.user")
	private String docbaseUser;

	@VersionProviderParam("docbase.password")
	private String docbasePassword;

	@VersionProviderParam("docbase.domain")
	private String docbaseDomain;

	private boolean checkTablesExist = true;

	private DocbaseSession session;

	@Override
	public void init(GenericTask task, Map<String, String> properties)
	{
		super.init(task, properties);

		//Either all params should be null or all of them should be specified
		if (docbaseUser == null && docbasePassword == null && docbaseName == null && docbaseDomain == null)
		{
			return;
		}

		if (docbaseUser == null)
		{
			throw new BuildException("Incorrect Documentum build version provider specification - user name should be specified");
		}

		if (docbasePassword == null)
		{
			throw new BuildException("Incorrect Documentum build version provider specification - user password should be specified");
		}

		if (docbaseName == null)
		{
			throw new BuildException("Incorrect Documentum build version provider specification - docbase name should be specified");
		}

		session = DocbaseSessionManager.newSession(new LoginInfo(docbaseUser, docbaseDomain, docbasePassword, docbaseName));

		Runtime.getRuntime().addShutdownHook(new DocbaseSessionsCleaner());
	}

	@Override
	public BuildVersion getCurrentVersion(String qualifier)
	{
		String query = "select " + majorCol + ", " + minorCol + ", " + patchCol +
				" from " + versionTable + " where " + qualifierCol + getQualifierWhereCondition(qualifier);

		try
		{
			IDfTypedObject obj = DqlHelper.getFirstString(getSession(), query);
			int major = obj.getInt(majorCol);
			int minor = obj.getInt(minorCol);
			int patch = obj.getInt(patchCol);

			return new BuildVersion(major, minor, patch);
		}
		catch (DfEndOfCollectionException e)
		{
			createVersionRecord(qualifier);
			return BuildVersion.INITIAL;
		}
		catch (DfException e)
		{
			if (e.getMessageId().equals(TYPE_NOT_EXIST_ERROR) ||
				e.getMessageId().equals(TABLE_NOT_FOUND_ERROR) ||
				e.getMessageId().equals(CURSOR_ERROR))
			{
				createNecessaryTables();
				createVersionRecord(qualifier);
				return BuildVersion.INITIAL;
			}

			throw new BuildException("Error occured while trying to get current Docbase version", e);
		}
	}

	@Override
	public boolean canUpdateToVersion(BuildVersion version, String qualifier)
	{
		//workaround for the situation when version tables or records is not already created
		if (checkTablesExist)
		{
			getCurrentVersion(qualifier);
			checkTablesExist = false;
		}

		//new and more accurate implementation
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

	private boolean isVersionHistoryRecordExist(BuildVersion buildVersion, String qualifier)
	{
		String query = "select * from " + historyTable +
				" where " + majorCol + "=" + buildVersion.major + " and " +
				minorCol + "=" + buildVersion.minor + " and " +
				patchCol + "=" + buildVersion.patch + " and " +
				qualifierCol + getQualifierWhereCondition(qualifier);

		try
		{
			return DqlHelper.exist(getSession(), query);
		}
		catch (DfException e)
		{
			throw new BuildException("Failed to check version history", e);
		}
	}

	private void createVersionRecord(String qualifier)
	{
		String query = "create " + versionTable + " object " +
				"set " + majorCol + "=0, " +
				"set " + minorCol + "=0, " +
				"set " + patchCol + "=0, " +
				"set " + qualifierCol + "='" + qualifier + "', " +
				"set " + updateTimeCol + "=DATE(NOW), " +
				"set " + docbaseUserCol + "='" + getSession().getUserLogin() + "', " +
				"set " + osUserCol + "='" + SystemHelper.osUser + "', " +
				"set " + hostnameCol + "='" + SystemHelper.hostName + "', " +
				"set " + ipCol + "='" + SystemHelper.hostIp + "'";

		try
		{
			DqlHelper.executeQuery(getSession(), query);
		}
		catch (DfException e)
		{
			throw new BuildException("Failed to create docbase version record, qualifier=" + qualifier, e);
		}
	}

	private void createNecessaryTables()
	{
		final String DQL_CHECK_VERSION_TABLE = "select " + majorCol + ", " +
				minorCol + ", " + patchCol + ", \"" + commentCol + "\", " + qualifierCol + ", " + updateTimeCol + ", " +
				docbaseUserCol + ", " + osUserCol + ", " + hostnameCol + ", " + ipCol +
				" from " + versionTable + " enable(return_top 1)";

		final String DQL_CHECK_HISTORY_TABLE = "select " + majorCol + ", " +
				minorCol + ", " + patchCol + ", " + qualifierCol + ", " + updateTimeCol + ", " +
				docbaseUserCol + ", " + osUserCol + ", " + hostnameCol + ", " + ipCol + ", " +
				prevMajorCol + ", " + prevMinorCol + ", " + prevPatchCol + ", \"" + commentCol + "\"" +
				" from " + historyTable + " enable(return_top 1)";

		final String DQL_CREATE_VERSION_TABLE = "create type \"" + versionTable + "\" " +
				"(\"" + majorCol + "\" integer, \"" + minorCol + "\" integer, " + "\"" + patchCol + "\" integer, " +
				"\"" + commentCol + "\" string(512), \"" + qualifierCol + "\" string(64), \"" + updateTimeCol + "\" date, " +
				"\"" + docbaseUserCol + "\" string(64), \"" + osUserCol + "\" string(64), " +
				"\"" + hostnameCol + "\" string(64), \"" + ipCol + "\" string(64)) " +
				"with supertype null publish";

		final String DQL_CREATE_HISTORY_TABLE = "create type \"" + historyTable + "\" " +
				"(\"" + majorCol + "\" integer, \"" + minorCol + "\" integer, " + "\"" + patchCol + "\" integer, " +
				"\"" + prevMajorCol + "\" integer, \"" + prevMinorCol + "\" integer, " + "\"" + prevPatchCol + "\" integer, " +
				"\"" + commentCol + "\" string(512), \"" + qualifierCol + "\" string(64), " +
				"\"" + updateTimeCol + "\" date, \"" + docbaseUserCol + "\" string(64), " +
				"\"" + osUserCol + "\" string(64), \"" + hostnameCol + "\" string(64), " +
				"\"" + ipCol + "\" string(64)) " +
				"with supertype null publish";

		boolean isCreated = false;

		if (!isTableExist(DQL_CHECK_VERSION_TABLE))
		{
			try
			{
				DqlHelper.executeQuery(getSession(), DQL_CREATE_VERSION_TABLE);
				isCreated = true;
			}
			catch (DfException e)
			{
				throw new BuildException("Failed to create version table", e);
			}
		}

		if (!isTableExist(DQL_CHECK_HISTORY_TABLE))
		{
			try
			{
				DqlHelper.executeQuery(getSession(), DQL_CREATE_HISTORY_TABLE);
				isCreated = true;
			}
			catch (DfException e)
			{
				throw new BuildException("Failed to create history table", e);
			}
		}

		if (isCreated)
		{
			getSession().flushCaches();
		}
	}

	private boolean isTableExist(String query)
	{
		try
		{
			DqlHelper.exist(getSession(), query);
			return true;
		}
		catch (DfException e)
		{
			if (e.getMessageId().equals(TYPE_NOT_EXIST_ERROR) ||
				e.getMessageId().equals(TABLE_NOT_FOUND_ERROR) ||
				e.getMessageId().equals(CURSOR_ERROR))
			{
				return false;
			}

			throw new BuildException("Failed to get current docbase version", e);
		}
	}

	private void updateVersionRecord(BuildVersion currentVersion, BuildVersion newVersion, String comment, String qualifier)
	{
		String query = "update " + versionTable + " object " +
				"set " + majorCol + "=" + newVersion.major + ", " +
				"set " + minorCol + "=" + newVersion.minor + ", " +
				"set " + patchCol + "=" + newVersion.patch + ", " +
				"set " + updateTimeCol + "=DATE(NOW), " +
				"set " + docbaseUserCol + "='" + getSession().getUserLogin() + "', " +
				"set " + osUserCol + "='" + SystemHelper.osUser + "', " +
				"set " + hostnameCol + "='" + SystemHelper.hostName + "', " +
				"set " + ipCol + "='" + SystemHelper.hostIp + "' " +
				"set \"" + commentCol + "\"='" + comment + "' " +
				"where " + majorCol + "=" + currentVersion.major + " and " +
				minorCol + "=" + currentVersion.minor + " and " +
				patchCol + "=" + currentVersion.patch + " and " +
				qualifierCol + getQualifierWhereCondition(qualifier);

		try
		{
			DqlHelper.executeQuery(getSession(), query);
		}
		catch (DfException e)
		{
			throw new BuildException("Failed to update docbase version to " +
					newVersion + (qualifier.isEmpty() ? "" : " [" + qualifier + "]"), e);
		}
	}

	private void createHistoryRecord(BuildVersion currentVersion, BuildVersion newVersion, String comment, String qualifier)
	{
		String query = "create " + historyTable + " object " +
				"set " + majorCol + "=" + newVersion.major + ", " +
				"set " + minorCol + "=" + newVersion.minor + ", " +
				"set " + patchCol + "=" + newVersion.patch + ", " +
				"set " + prevMajorCol + "=" + currentVersion.major + ", " +
				"set " + prevMinorCol + "=" + currentVersion.minor + ", " +
				"set " + prevPatchCol + "=" + currentVersion.patch + ", " +
				"set \"" + commentCol + "\"='" + comment + "', " +
				"set " + qualifierCol + "='" + qualifier + "', " +
				"set " + updateTimeCol + "=DATE(NOW), " +
				"set " + docbaseUserCol + "='" + getSession().getUserLogin() + "', " +
				"set " + osUserCol + "='" + SystemHelper.osUser + "', " +
				"set " + hostnameCol + "='" + SystemHelper.hostName + "', " +
				"set " + ipCol + "='" + SystemHelper.hostIp + "'";

		try
		{
			DqlHelper.executeQuery(getSession(), query);
		}
		catch (DfException e)
		{
			throw new BuildException("Failed to create docbase version history for " +
					currentVersion + "->"  + newVersion + (qualifier.isEmpty() ? "" : " [" + qualifier + "]"), e);
		}
	}

	private String getQualifierWhereCondition(String qualifier)
	{
		return qualifier == null || qualifier.trim().length() == 0 ?
				" is nullstring" : "='" + qualifier + "'";
	}

	private DocbaseSession getSession()
	{
		return session != null ? session : DocbaseSessionManager.getSession();
	}
}
