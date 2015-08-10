package com.anttoolkit.hbase.tasks.table;

import java.io.*;

import com.anttoolkit.hbase.tasks.GenericHBaseTask;
import org.apache.tools.ant.*;

public class DropTableTask extends GenericHBaseTask
{
	private String table;

	public void setTable(String table)
	{
		this.table = table;
	}

	@Override
	protected void doHadoopWork() throws BuildException
	{
		this.dropTable(table);
		log("HBase table " + table + " was dropped");
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
