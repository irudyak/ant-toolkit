package com.anttoolkit.hbase.tasks.table;

import org.apache.tools.ant.*;

import com.anttoolkit.hbase.tasks.*;

public class MajorCompactTableTask extends GenericHBaseTask
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
				throw new BuildException("Table " + table + " doesn't exist");
			}

			if (columnFamily != null)
			{
				getHBaseAdmin().majorCompact(table, columnFamily);
			}
			else
			{
				getHBaseAdmin().majorCompact(table);
			}
		}
		catch (Exception e)
		{
			if (columnFamily != null)
			{
				throw new BuildException("Failed to major compact " + columnFamily + " column family of HBase table " + table);
			}
			else
			{
				throw new BuildException("Failed to major compact HBase table " + table);
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
	}
}
