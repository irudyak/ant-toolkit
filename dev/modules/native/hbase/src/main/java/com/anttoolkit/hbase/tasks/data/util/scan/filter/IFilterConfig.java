package com.anttoolkit.hbase.tasks.data.util.scan.filter;

import org.apache.hadoop.hbase.filter.*;

import com.anttoolkit.general.tasks.*;

public interface IFilterConfig
{
	public Filter getFilter(GenericTask task);
	public void validate(GenericTask task);
}
