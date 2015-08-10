package com.anttoolkit.hbase.tasks.data.util.scan.filter;

import org.apache.hadoop.hbase.filter.*;

import com.anttoolkit.general.tasks.*;

public class QualifierFilterConfig extends ComparatorFilterConfig
{
	@Override
	public Filter getFilter(GenericTask task)
	{
		validate(task);
		return new QualifierFilter(getOperation(), getComparator(task));
	}
}
