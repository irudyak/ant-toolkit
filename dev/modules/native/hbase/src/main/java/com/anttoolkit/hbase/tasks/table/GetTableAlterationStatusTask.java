package com.anttoolkit.hbase.tasks.table;

import java.io.*;

import org.apache.hadoop.hbase.*;
import org.apache.hadoop.hbase.util.*;
import org.apache.tools.ant.*;

import com.anttoolkit.hbase.tasks.*;

public class GetTableAlterationStatusTask extends GenericHBaseTask
{
	private String table;
	private String totalRegionsProperty;
	private String inProgressRegionsProperty;

	public void setTable(String table)
	{
		this.table = table;
	}

	public void setTotalRegionsProperty(String property)
	{
		totalRegionsProperty = property;
	}

	public void setInProgressRegionsProperty(String property)
	{
		inProgressRegionsProperty = property;
	}

	@Override
	protected void doHadoopWork() throws BuildException
	{
		try
		{
			Pair<Integer, Integer> pair = getHBaseAdmin().getAlterStatus(TableName.valueOf(table));

			if (totalRegionsProperty != null)
			{
				setPropertyThreadSafe(totalRegionsProperty, Integer.toString(pair.getSecond()));
			}

			if (inProgressRegionsProperty != null)
			{
				setPropertyThreadSafe(inProgressRegionsProperty, Integer.toString(pair.getFirst()));
			}
		}
		catch (IOException e)
		{
			throw new BuildException("Failed to get alter status for table " + table, e);
		}
	}

	@Override
	protected void hadoopValidate()
	{
		if (table == null || table.trim().isEmpty())
		{
			throw new BuildException("Table name should be specified");
		}

		try
		{
			if (!getHBaseAdmin().tableExists(table) )
			{
				throw new BuildException("HBase table " + table + " doesn't exist");
			}
		}
		catch (IOException e)
		{
			throw new BuildException("Failed to check if HBase table " + table + " already exists", e);
		}
	}
}
