package com.anttoolkit.hbase.tasks.table;

import java.io.*;

import org.apache.tools.ant.*;

import com.anttoolkit.hbase.tasks.*;

public class SplitTableTask extends GenericHBaseTask
{
	private String table;
	private String splitPoint;

	public void setTable(String table)
	{
		this.table = table;
	}

	public void setSplitPoint(String splitPoint)
	{
		this.splitPoint = splitPoint;
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

			if (splitPoint != null)
			{
				getHBaseAdmin().split(table, splitPoint);
			}
			else
			{
				getHBaseAdmin().split(table);
			}
		}
		catch (Exception e)
		{
			throw new BuildException("Failed to split table " + table);
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
