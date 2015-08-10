package com.anttoolkit.hbase.tasks.table;

import java.io.*;

import org.apache.tools.ant.*;

import com.anttoolkit.hbase.tasks.*;

public class CompactTableTask extends GenericHBaseTask
{
	private String table;
	private String columnFamily;


	public void setTable(String table)
	{
		this.table = table;
	}

	public void setColumnFamily(String columnFamily)
	{
		this.columnFamily = columnFamily;
	}

	@Override
	protected void doHadoopWork() throws BuildException
	{
		try
		{
			if (!getHBaseAdmin().tableExists(table))
			{
				throw new BuildException("Can't compact table " + table + " cause it doesn't exist");
			}

			if (columnFamily != null)
			{
				try
				{
					getHBaseAdmin().compact(table, columnFamily);
				}
				catch (InterruptedException e)
				{
					throw new BuildException("Failed to compact column family " + columnFamily + " of " + table + " table");
				}
			}
			else
			{
				try
				{
					getHBaseAdmin().compact(table);
				}
				catch (InterruptedException e)
				{
					throw new BuildException("Failed to compact " + table + " table");
				}
			}
		}
		catch (IOException e)
		{
			throw new BuildException("Failed to check existent of the table " + table, e);
		}
	}

	@Override
	protected void hadoopValidate()
	{
		if (table == null || table.trim().isEmpty())
		{
			throw new BuildException("Table name should be specified");
		}
	}
}
