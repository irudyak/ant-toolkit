package com.anttoolkit.hbase.tasks.data.util.scan.filter;

import org.apache.hadoop.hbase.filter.*;
import org.apache.hadoop.hbase.util.*;
import org.apache.tools.ant.*;

import com.anttoolkit.general.tasks.*;

public class DependentColumnFilterConfig extends ComparatorFilterConfig
{
	private String family;
	private String column;
	private boolean dropDependent = false;

	public void setColumnFamily(String family)
	{
		this.family = family;
	}

	public void setColumn(String column)
	{
		this.column = column;
	}

	protected void setDropDependent(boolean drop)
	{
		this.dropDependent = drop;
	}

	@Override
	public Filter getFilter(GenericTask task)
	{
		validate(task);
		return new DependentColumnFilter(Bytes.toBytes(family), Bytes.toBytes(column),
				dropDependent, getOperation(), getComparator(task));
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
