package com.anttoolkit.hbase.tasks.data.util.scan.filter;

import org.apache.hadoop.hbase.filter.*;
import org.apache.tools.ant.*;

import com.anttoolkit.general.tasks.*;

public class PageFilterConfig implements IFilterConfig
{
	private Long pageSize;

	public void setPageSize(long pageSize)
	{
		this.pageSize = pageSize;
	}

	@Override
	public Filter getFilter(GenericTask task)
	{
		validate(task);
		return new PageFilter(pageSize);
	}

	@Override
	public void validate(GenericTask task)
	{
		if (pageSize == null)
		{
			throw new BuildException("Page size should be specified");
		}
	}
}
