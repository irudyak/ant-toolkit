package com.anttoolkit.hbase.tasks.table;

import java.io.*;

import com.anttoolkit.hbase.tasks.GenericHBaseTask;
import org.apache.hadoop.hbase.*;
import org.apache.hadoop.hbase.util.*;
import org.apache.tools.ant.*;

import com.anttoolkit.general.tasks.collections.array.util.*;

public class ListTablesTask extends GenericHBaseTask
{
	private String array;
	private String filter;

	public void setArray(String array)
	{
		this.array = array;
	}

	public void setFilter(String filter)
	{
		this.filter = filter;
	}

	@Override
	protected void doHadoopWork() throws BuildException
	{
		try
		{
			HTableDescriptor[] tables = filter != null ?
					getHBaseAdmin().listTables(filter) :
					getHBaseAdmin().listTables();

			if (tables == null)
			{
				return;
			}

			for (HTableDescriptor table : tables)
			{
				ArrayManager.add(array, Bytes.toString(table.getName()));
			}
		}
		catch (IOException e)
		{
			throw new BuildException("Failed to list HBase tables", e);
		}
	}

	@Override
	protected void hadoopValidate()
	{
		if (array == null)
		{
			throw new BuildException("Array name should be specified");
		}

		if (!ArrayManager.exists(array))
		{
			throw new BuildException("Specified array " + array + " doesn't exist");
		}
	}
}
