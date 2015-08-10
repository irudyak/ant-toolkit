package com.anttoolkit.hbase.tasks.table;

import java.io.*;

import com.anttoolkit.hbase.tasks.GenericHBaseTask;
import org.apache.tools.ant.*;

public class CheckTableExistsTask extends GenericHBaseTask
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
			this.setPropertyThreadSafe(property, Boolean.toString(getHBaseAdmin().tableExists(table)));
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

		if (property == null || property.trim().isEmpty())
		{
			throw new BuildException("Property should be specified");
		}
	}
}
