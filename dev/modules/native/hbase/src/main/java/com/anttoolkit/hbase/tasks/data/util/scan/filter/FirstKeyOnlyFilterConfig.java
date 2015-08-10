package com.anttoolkit.hbase.tasks.data.util.scan.filter;

import org.apache.hadoop.hbase.filter.*;

import com.anttoolkit.general.tasks.*;

public class FirstKeyOnlyFilterConfig implements IFilterConfig
{
	@Override
	public Filter getFilter(GenericTask task)
	{
		return new FirstKeyOnlyFilter();
	}

	@Override
	public void validate(GenericTask task)
	{
	}
}
