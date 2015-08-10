package com.anttoolkit.hbase.tasks.table;

import java.io.*;

import com.anttoolkit.hbase.tasks.GenericHBaseTask;
import org.apache.tools.ant.*;

public class EnableTableTask extends GenericHBaseTask
{
	private String table;

	public void setTable(String table)
	{
		this.table = table;
	}

	@Override
	protected void doHadoopWork() throws BuildException
	{
		this.enableTable(table);
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
