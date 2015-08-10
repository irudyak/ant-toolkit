package com.anttoolkit.hbase.tasks.data;

import java.util.*;

import org.apache.hadoop.hbase.client.*;
import org.apache.hadoop.hbase.util.*;
import org.apache.tools.ant.*;

import com.anttoolkit.hbase.tasks.*;
import com.anttoolkit.general.tasks.collections.array.util.*;

public class GetResultTimestampsTask extends GenericHBaseTask
{
	private String columnFamily;
	private String column;
	private String array;
	private String ref;

	public void setColumnFamily(String columnFamily)
	{
		this.columnFamily = columnFamily;
	}

	public void setColumn(String column)
	{
		this.column = column;
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

		NavigableMap<Long, byte[]> versionsMap = columnsMap.get(Bytes.toBytes(column));
		if (versionsMap == null)
		{
			throw new BuildException("Specified result '" + ref +
					"' of GET operation doesn't contain '" + columnFamily + ":" + column + "' column");
		}

		Set<Long> versions = versionsMap.keySet();
		for (Long version : versions)
		{
			ArrayManager.add(array, version.toString());
		}
	}

	@Override
	protected void hadoopValidate()
	{
		if (columnFamily == null || columnFamily.trim().isEmpty() ||
			column == null || column.trim().isEmpty())
		{
			throw new BuildException("Column family and column from which to extract version stamps should be specified");
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

		Object obj = this.getReference(ref);
		if (obj == null || !(obj instanceof Result))
		{
			throw new BuildException("Reference '" + ref + "' to a RESULT object, contains incorrect object");
		}
	}
}
