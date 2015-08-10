package com.anttoolkit.hbase.tasks.namespace;

import java.io.*;

import org.apache.hadoop.hbase.*;
import org.apache.tools.ant.*;

import com.anttoolkit.hbase.tasks.*;
import com.anttoolkit.general.tasks.collections.array.util.*;

public class GetNamespaceTablesTask extends GenericHBaseTask
{
	private String namespace;
	private String array;

	public void setNamespace(String namespace)
	{
		this.namespace = namespace;
	}

	public void setArray(String array)
	{
		this.array = array;
	}

	@Override
	protected void doHadoopWork() throws BuildException
	{
		TableName[] tables;

		try
		{
			tables = getHBaseAdmin().listTableNamesByNamespace(namespace);
		}
		catch (IOException e)
		{
			throw new BuildException("Failed to list tables from namespace " + namespace, e);
		}

		if (tables == null)
		{
			return;
		}

		for (TableName table : tables)
		{
			ArrayManager.add(array, table.getNameAsString());
		}
	}

	@Override
	protected void hadoopValidate()
	{
		if (namespace == null || namespace.trim().isEmpty())
		{
			throw new BuildException("Namespace where to look for HBase tables should be specified");
		}

		if (array == null || array.trim().isEmpty())
		{
			throw new BuildException("Array name to store HBase tables should be specified");
		}

		if (!ArrayManager.exists(array))
		{
			throw new BuildException("Array " + array + " doesn't exist");
		}
	}
}
