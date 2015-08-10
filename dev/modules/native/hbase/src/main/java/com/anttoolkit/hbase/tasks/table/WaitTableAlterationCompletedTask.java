package com.anttoolkit.hbase.tasks.table;

import java.io.*;

import org.apache.tools.ant.*;

import com.anttoolkit.hbase.tasks.*;

public class WaitTableAlterationCompletedTask extends GenericHBaseTask
{
	private static final long SLEEP_TIMEOUT = 10000;

	private String table;

	public void setTable(String table)
	{
		this.table = table;
	}

	@Override
	protected void doHadoopWork() throws BuildException
	{
		while (!this.checkTableAlterationCompleted(table))
		{
			try
			{
				Thread.sleep(SLEEP_TIMEOUT);
			}
			catch (InterruptedException e)
			{
				throw new BuildException("HBase table " + table + " alteration wait was interrupted", e);
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
