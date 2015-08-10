package com.anttoolkit.hbase.tasks.table;

import org.apache.tools.ant.*;

import com.anttoolkit.hbase.tasks.*;

public class FlushTableTask extends GenericHBaseTask
{
	private String table;

	public void setTable(String table)
	{
		this.table = table;
	}

	@Override
	protected void doHadoopWork() throws BuildException
	{
		try
		{
			if (getHBaseAdmin().tableExists(table))
			{
				throw new BuildException("HBase table " + table + " doesn't exist");
			}

			getHBaseAdmin().flush(table);
		}
		catch (Exception e)
		{
			throw new BuildException("Failed to flash table " + table, e);
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
