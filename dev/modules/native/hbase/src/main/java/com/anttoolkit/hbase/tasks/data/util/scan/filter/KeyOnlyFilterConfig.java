package com.anttoolkit.hbase.tasks.data.util.scan.filter;

import org.apache.hadoop.hbase.filter.*;

import com.anttoolkit.general.tasks.*;

public class KeyOnlyFilterConfig implements IFilterConfig
{
	@Override
	public Filter getFilter(GenericTask task)
	{
		return new KeyOnlyFilter();
	}

	@Override
	public void validate(GenericTask task)
	{
	}
}
