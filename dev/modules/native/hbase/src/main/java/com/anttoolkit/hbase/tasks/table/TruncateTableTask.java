package com.anttoolkit.hbase.tasks.table;

import java.io.*;

import org.apache.hadoop.hbase.*;
import org.apache.hadoop.hbase.client.*;
import org.apache.tools.ant.*;

import com.anttoolkit.hbase.tasks.*;

public class TruncateTableTask extends GenericHBaseTask
{
	private String table;

	public void setTable(String table)
	{
		this.table = table;
	}

	@Override
	protected void doHadoopWork() throws BuildException
	{
		HTableDescriptor htableDescr;

		try
		{
			htableDescr = getTable(table).getTableDescriptor();
		}
		catch (IOException e)
		{
			throw new BuildException("Failed to get descriptor for HBase table " + table, e);
		}

		this.dropTable(table);

		try
		{
			getHBaseAdmin().createTable(htableDescr);
		}
		catch (IOException e)
		{
			throw new BuildException("Failed to recreate previously dropped table " + table);
		}

		log("HBase table " + table + " was truncated");
	}

	@Override
	protected void hadoopValidate()
	{
		if (table == null || table.trim().isEmpty())
		{
			throw new BuildException("Table name to truncate should be specified");
		}

		try
		{
			if (!getHBaseAdmin().tableExists(table))
			{
				throw new BuildException("Specified HBase table " + table + " doesn't exist");
			}
		}
		catch (IOException e)
		{
			throw new BuildException("Failed to check if HBase table " + table + " exists", e);
		}
	}
}
