package com.anttoolkit.hbase.tasks.data.util.scan.filter;

import org.apache.hadoop.hbase.filter.*;
import org.apache.tools.ant.*;

import com.anttoolkit.general.tasks.*;

public class ColumnCountGetFilterConfig implements IFilterConfig
{
	private Integer limit;

	public void setLimit(int limit)
	{
		this.limit = limit;
	}

	@Override
	public Filter getFilter(GenericTask task)
	{
		validate(task);
		return new ColumnCountGetFilter(limit);
	}

	@Override
	public void validate(GenericTask task)
	{
		if (limit == null)
		{
			throw new BuildException("Limit should be specified");
		}
	}
}
