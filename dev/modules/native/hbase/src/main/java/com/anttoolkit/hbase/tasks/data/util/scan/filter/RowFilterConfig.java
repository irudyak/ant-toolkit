package com.anttoolkit.hbase.tasks.data.util.scan.filter;

import org.apache.hadoop.hbase.filter.*;

import com.anttoolkit.general.tasks.*;

public class RowFilterConfig extends ComparatorFilterConfig
{
	@Override
	public Filter getFilter(GenericTask task)
	{
		validate(task);
		return new RowFilter(getOperation(), getComparator(task));
	}
}
