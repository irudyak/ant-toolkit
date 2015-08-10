package com.anttoolkit.hbase.tasks.table;

import java.io.*;

import com.anttoolkit.hbase.tasks.GenericHBaseTask;
import org.apache.hadoop.hbase.*;
import org.apache.tools.ant.*;

public class DescribeTableTask extends GenericHBaseTask
{
	private String table;
	private String property;

	public void setTable(String table)
	{
		this.table = table;
	}

	public void setProperty(String property)
	{
		this.property = property;
	}

	@Override
	protected void doHadoopWork() throws BuildException
	{
		try
		{
			if (property != null)
			{
				this.setPropertyThreadSafe(property, getHBaseAdmin().getTableDescriptor(TableName.valueOf(table)).toString());
			}
			else
			{
				log(getHBaseAdmin().getTableDescriptor(TableName.valueOf(table)).toString());
			}
		}
		catch (IOException e)
		{
			throw new BuildException("Failed to describe table " + table, e);
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
