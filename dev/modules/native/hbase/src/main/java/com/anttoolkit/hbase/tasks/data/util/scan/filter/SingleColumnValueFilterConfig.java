package com.anttoolkit.hbase.tasks.data.util.scan.filter;

import org.apache.hadoop.hbase.filter.*;
import org.apache.hadoop.hbase.util.*;
import org.apache.tools.ant.*;

import com.anttoolkit.general.tasks.*;

public class SingleColumnValueFilterConfig extends ComparatorFilterConfig
{
	private String family;
	private String column;

	public void setColumnFamily(String family)
	{
		this.family = family;
	}

	public void setColumn(String column)
	{
		this.column = column;
	}

	@Override
	public Filter getFilter(GenericTask task)
	{
		validate(task);
		return new SingleColumnValueFilter(Bytes.toBytes(family), Bytes.toBytes(column),
				getOperation(), getComparator(task));
	}

	@Override
	public void validate(GenericTask task)
	{
		super.validate(task);

		if (family == null)
		{
			throw new BuildException("Column family should be specified");
		}

		if (column == null)
		{
			throw new BuildException("Column should be specified");
		}
	}
}
