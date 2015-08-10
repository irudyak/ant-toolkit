package com.anttoolkit.hbase.tasks.table;

import java.io.*;
import java.util.*;

import com.anttoolkit.hbase.tasks.GenericHBaseTask;
import org.apache.hadoop.hbase.*;
import org.apache.tools.ant.*;

import com.anttoolkit.general.tasks.collections.array.util.*;

public class GetTableRegionsTask extends GenericHBaseTask
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
		List<HRegionInfo> regions;

		try
		{
			regions = getHBaseAdmin().getTableRegions(TableName.valueOf(table));
		}
		catch (IOException e)
		{
			throw new BuildException("Failed to get regions for HBase table " + table);
		}

		if (regions == null)
		{
			return;
		}

		for (HRegionInfo region : regions)
		{
			ArrayManager.add(array, region.getRegionNameAsString());
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
			throw new BuildException("There is no array specified for HBase table regions");
		}

		if (!ArrayManager.exists(array))
		{
			throw new BuildException("Array " + array + " doesn't exist");
		}
	}
}
