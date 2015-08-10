package com.anttoolkit.hbase.tasks.data.util.scan.filter;

import org.apache.hadoop.hbase.filter.*;

import com.anttoolkit.general.tasks.*;

public class ValueFilterConfig extends ComparatorFilterConfig
{
	@Override
	public Filter getFilter(GenericTask task)
	{
		return new ValueFilter(getOperation(), getComparator(task));
	}
}
