package com.anttoolkit.hbase.tasks.snapshot;

import java.io.*;
import java.util.*;

import org.apache.hadoop.hbase.protobuf.generated.HBaseProtos;
import org.apache.tools.ant.*;

import com.anttoolkit.hbase.tasks.*;
import com.anttoolkit.general.tasks.collections.array.util.*;

public class GetTableSnapshotsTask extends GenericHBaseTask
{
	private String table;
	private String array;

	public void setTable(String table)
	{
		this.table = table;
	}

	public void setArray(String array)
	{
		this.array = array;
	}

	@Override
	protected void doHadoopWork() throws BuildException
	{
		List<HBaseProtos.SnapshotDescription> snapshots;

		try
		{
			snapshots = getHBaseAdmin().listSnapshots();
		}
		catch (IOException e)
		{
			throw new BuildException("Failed to get list of available snapshots");
		}

		if (snapshots == null)
		{
			return;
		}

		for (HBaseProtos.SnapshotDescription snapshot : snapshots)
		{
			if (snapshot.getTable().equals(table))
			{
				ArrayManager.add(array, snapshot.getName());
			}
		}
	}

	@Override
	protected void hadoopValidate()
	{
		if (table == null || table.trim().isEmpty())
		{
			throw new BuildException("Table name should be specified");
		}

		if (array == null || array.trim().isEmpty())
		{
			throw new BuildException("There is no array specified for HBase table snapshots");
		}

		if (!ArrayManager.exists(array))
		{
			throw new BuildException("Array " + array + " doesn't exist");
		}
	}
}
