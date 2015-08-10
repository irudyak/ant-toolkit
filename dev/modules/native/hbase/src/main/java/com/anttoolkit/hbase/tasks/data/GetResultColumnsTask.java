package com.anttoolkit.hbase.tasks.data;

import java.util.*;

import org.apache.hadoop.hbase.client.*;
import org.apache.hadoop.hbase.util.*;
import org.apache.tools.ant.*;

import com.anttoolkit.hbase.tasks.*;
import com.anttoolkit.general.tasks.collections.array.util.*;

public class GetResultColumnsTask extends GenericHBaseTask
{
	private String columnFamily;
	private String array;
	private String ref;

	public void setColumnFamily(String columnFamily)
	{
		this.columnFamily = columnFamily;
	}

	public void setArray(String array)
	{
		this.array = array;
	}

	public void setRefid(String reference)
	{
		this.ref = reference;
	}

	@Override
	protected void doHadoopWork() throws BuildException
	{
		Result result = (Result)this.getReference(ref);

		NavigableMap<byte[], NavigableMap<Long, byte[]>> columnsMap = result.getMap().get(Bytes.toBytes(columnFamily));
		if (columnsMap == null)
		{
			throw new BuildException("Specified result '" + ref +
					"' of GET operation doesn't contain '" + columnFamily + "' column family");
		}

		Set<byte[]> columns = columnsMap.keySet();
		for (byte[] column : columns)
		{
			ArrayManager.add(array, Bytes.toString(column));
		}
	}

	@Override
	protected void hadoopValidate()
	{
		if (columnFamily == null || columnFamily.trim().isEmpty())
		{
			throw new BuildException("Column family from which to extract columns should be specified");
		}

		if (array == null)
		{
			throw new BuildException("Array to store columns should be specified");
		}

		if (!ArrayManager.exists(array))
		{
			throw new BuildException("Specified array '" + array + "' doesn't exist");
		}

		if (ref == null)
		{
			throw new BuildException("Reference to a RESULT object wasn't specified");
		}

		Object obj = getReference(ref);
		if (obj == null ||
			!(obj instanceof Result))
		{
			throw new BuildException("Reference '" + ref + "' to a RESULT object, contains incorrect object");
		}
	}
}
