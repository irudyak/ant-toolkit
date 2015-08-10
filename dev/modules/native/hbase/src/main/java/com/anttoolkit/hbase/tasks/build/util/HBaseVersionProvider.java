package com.anttoolkit.hbase.tasks.build.util;

import java.io.*;

import org.apache.hadoop.hbase.client.*;
import org.apache.hadoop.hbase.util.*;
import org.apache.hadoop.hbase.*;
import org.apache.tools.ant.*;

import com.anttoolkit.hbase.common.*;
import com.anttoolkit.general.tasks.build.util.*;
import com.anttoolkit.general.common.*;

public class HBaseVersionProvider extends GenericVersionProvider
{
	//namespace to store tables
	@VersionProviderParam("namespace")
	protected String namespace = "BUILD";

	//tables
	@VersionProviderParam("version.table")
	protected String versionTable = "BUILD_VERSION";

	@VersionProviderParam("history.table")
	protected String historyTable = "BUILD_HISTORY";

	//column families
	@VersionProviderParam("column.family")
	protected String columnFamily = "DETAILS";

	//attributes
	@VersionProviderParam("major.column")
	private String majorCol = "MAJOR";

	@VersionProviderParam("minor.column")
	private String minorCol = "MINOR";

	@VersionProviderParam("patch.column")
	private String patchCol = "PATCH";

	@VersionProviderParam("update.time.column")
	private String updateTimeCol = "UPDATE_TIME";

	@VersionProviderParam("qualifier.column")
	private String qualifierCol = "QUALIFIER";

	@VersionProviderParam("osuser.column")
	private String osUserCol = "OS_USER";

	@VersionProviderParam("hostname.column")
	private String hostnameCol = "HOSTNAME";

	@VersionProviderParam("ip.column")
	private String ipCol = "IP";

	//history attributes
	@VersionProviderParam("prev.major.column")
	private String prevMajorCol = "MAJOR_PREV";

	@VersionProviderParam("prev.minor.column")
	private String prevMinorCol = "MINOR_PREV";

	@VersionProviderParam("prev.patch.column")
	private String prevPatchCol = "PATCH_PREV";

	@VersionProviderParam("comment.column")
	private String commentCol = "COMMENT";

	private boolean checkTablesExist = true;

	@Override
	public BuildVersion getCurrentVersion(String qualifier)
	{
		Result result;
		String rowkey = getVersionRecordRowkey(qualifier);

		try
		{
			result = getTable(fullName(versionTable)).get(new Get(Bytes.toBytes(rowkey)));
		}
		catch (IOException e)
		{
			throw new BuildException("Failed to get data from HBase table '" + fullName(versionTable) + "' using rowkey '" + rowkey + "'", e);
		}

		if (result == null || result.isEmpty())
		{
			updateVersionRecord(BuildVersion.INITIAL, "", qualifier, System.currentTimeMillis());
			return BuildVersion.INITIAL;
		}

		int major = Bytes.toInt(result.getValue(Bytes.toBytes(columnFamily), Bytes.toBytes(majorCol)));
		int minor = Bytes.toInt(result.getValue(Bytes.toBytes(columnFamily), Bytes.toBytes(minorCol)));
		int patch = Bytes.toInt(result.getValue(Bytes.toBytes(columnFamily), Bytes.toBytes(patchCol)));

		return new BuildVersion(major, minor, patch);
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

		boolean outOfTurnUpdate = newVersion.compareTo(currentVersion) < 0;

		comment = comment == null ? "" : comment;

		long timestamp = System.currentTimeMillis();

		if (!outOfTurnUpdate)
		{
			updateVersionRecord(newVersion, comment, qualifier, timestamp);
		}

		createHistoryRecord(currentVersion, newVersion, comment, qualifier, timestamp);
	}

	private HBaseResourcesProvider getHBaseProvider()
	{
		HBaseResourcesProvider provider = HBaseResourcesManager.getProvider();
		if (provider == null)
		{
			throw new BuildException("There are no HBase connection specified");
		}

		return provider;
	}

	private HTableInterface getTable(String name)
	{
		try
		{
			return getHBaseProvider().getTable(name);
		}
		catch (Throwable e)
		{
			if (!(e.getCause() instanceof TableNotFoundException))
			{
				throw new BuildException("Failed to check HBase table: " + name, e);
			}

			return createTable(name);
		}
	}

	private HTableInterface createTable(String name)
	{
		createNamespace();

		HColumnDescriptor column = new HColumnDescriptor(Bytes.toBytes(columnFamily));
		column.setMaxVersions(1);

		HTableDescriptor htable = new HTableDescriptor(TableName.valueOf(name));
		htable.addFamily(column);

		try
		{
			getHBaseProvider().getAdmin().createTable(htable);
		}
		catch (IOException e)
		{
			if (!(e instanceof TableExistsException))
			{
				throw new BuildException("Failed to create HBase table " + name, e);
			}
		}

		return getHBaseProvider().getTable(name);
	}

	private void createNamespace()
	{
		NamespaceDescriptor[] descriptors;

		try
		{
			descriptors = getHBaseProvider().getAdmin().listNamespaceDescriptors();
		}
		catch (IOException e)
		{
			throw new BuildException("Failed to get list of namespace descriptors");
		}

		if (descriptors != null)
		{
			for (NamespaceDescriptor descriptor : descriptors)
			{
				if (descriptor.getName().equals(namespace))
				{
					return;
				}
			}
		}

		try
		{
			NamespaceDescriptor.Builder builder = NamespaceDescriptor.create(namespace);
			getHBaseProvider().getAdmin().createNamespace(builder.build());
		}
		catch (IOException e)
		{
			if (!(e instanceof NamespaceExistException))
			{
				throw new BuildException("Failed to create HBase namespace '" + namespace + "'", e);
			}
		}
	}

	private boolean isVersionHistoryRecordExist(BuildVersion buildVersion, String qualifier)
	{
		String rowkey = getVersionHistoryRecordRowkey(buildVersion, qualifier);

		Result result;

		try
		{
			result = getTable(fullName(historyTable)).get(new Get(Bytes.toBytes(rowkey)));
		}
		catch (IOException e)
		{
			throw new BuildException("Failed to get data from HBase table '" + fullName(historyTable) + "' using rowkey '" + rowkey + "'", e);
		}

		return result != null && !result.isEmpty();
	}

	private String getVersionRecordRowkey(String qualifier)
	{
		return qualifier == null || qualifier.trim().length() == 0 ? "version" : "version_" + qualifier;
	}

	private String getVersionHistoryRecordRowkey(BuildVersion version, String qualifier)
	{
		qualifier = qualifier == null || qualifier.trim().isEmpty() ? "" : qualifier + "_";
		return qualifier + version.toString();
	}

	private void updateVersionRecord(BuildVersion newVersion, String comment, String qualifier, long timestamp)
	{
		qualifier = qualifier == null ? "" : qualifier;

		byte[] columnFamily = Bytes.toBytes(this.columnFamily);

		Put put = new Put(Bytes.toBytes(getVersionRecordRowkey(qualifier)));
		put.addImmutable(columnFamily, Bytes.toBytes(majorCol), Bytes.toBytes(newVersion.major));
		put.addImmutable(columnFamily, Bytes.toBytes(minorCol), Bytes.toBytes(newVersion.minor));
		put.addImmutable(columnFamily, Bytes.toBytes(patchCol), Bytes.toBytes(newVersion.patch));
		put.addImmutable(columnFamily, Bytes.toBytes(updateTimeCol), Bytes.toBytes(timestamp));
		put.addImmutable(columnFamily, Bytes.toBytes(qualifierCol), Bytes.toBytes(qualifier));
		put.addImmutable(columnFamily, Bytes.toBytes(osUserCol), Bytes.toBytes(SystemHelper.osUser));
		put.addImmutable(columnFamily, Bytes.toBytes(hostnameCol), Bytes.toBytes(SystemHelper.hostName));
		put.addImmutable(columnFamily, Bytes.toBytes(ipCol), Bytes.toBytes(SystemHelper.hostIp));
		put.addImmutable(columnFamily, Bytes.toBytes(commentCol), Bytes.toBytes(comment));

		try
		{
			getTable(fullName(versionTable)).put(put);
		}
		catch (IOException e)
		{
			if (qualifier.isEmpty())
			{
				throw new BuildException("Failed to update version record to " + newVersion);
			}

			throw new BuildException("Failed to update version record for qualifier '" + qualifier + "' to " + newVersion);
		}
	}

	private void createHistoryRecord(BuildVersion currentVersion, BuildVersion newVersion, String comment, String qualifier, long timestamp)
	{
		qualifier = qualifier == null ? "" : qualifier;

		byte[] columnFamily = Bytes.toBytes(this.columnFamily);

		Put put = new Put(Bytes.toBytes(getVersionHistoryRecordRowkey(newVersion, qualifier)));
		put.addImmutable(columnFamily, Bytes.toBytes(majorCol), Bytes.toBytes(newVersion.major));
		put.addImmutable(columnFamily, Bytes.toBytes(minorCol), Bytes.toBytes(newVersion.minor));
		put.addImmutable(columnFamily, Bytes.toBytes(patchCol), Bytes.toBytes(newVersion.patch));
		put.addImmutable(columnFamily, Bytes.toBytes(prevMajorCol), Bytes.toBytes(currentVersion.major));
		put.addImmutable(columnFamily, Bytes.toBytes(prevMinorCol), Bytes.toBytes(currentVersion.minor));
		put.addImmutable(columnFamily, Bytes.toBytes(prevPatchCol), Bytes.toBytes(currentVersion.patch));
		put.addImmutable(columnFamily, Bytes.toBytes(updateTimeCol), Bytes.toBytes(timestamp));
		put.addImmutable(columnFamily, Bytes.toBytes(qualifierCol), Bytes.toBytes(qualifier));
		put.addImmutable(columnFamily, Bytes.toBytes(osUserCol), Bytes.toBytes(SystemHelper.osUser));
		put.addImmutable(columnFamily, Bytes.toBytes(hostnameCol), Bytes.toBytes(SystemHelper.hostName));
		put.addImmutable(columnFamily, Bytes.toBytes(ipCol), Bytes.toBytes(SystemHelper.hostIp));
		put.addImmutable(columnFamily, Bytes.toBytes(commentCol), Bytes.toBytes(comment));

		try
		{
			getTable(fullName(historyTable)).put(put);
		}
		catch (IOException e)
		{
			if (qualifier.isEmpty())
			{
				throw new BuildException("Failed to update version record to " + newVersion);
			}

			throw new BuildException("Failed to update version record for qualifier '" + qualifier + "' to " + newVersion);
		}
	}

	private String fullName(String table)
	{
		if (namespace == null || namespace.trim().isEmpty())
		{
			return table;
		}

		return namespace + ":" + table;
	}
}
