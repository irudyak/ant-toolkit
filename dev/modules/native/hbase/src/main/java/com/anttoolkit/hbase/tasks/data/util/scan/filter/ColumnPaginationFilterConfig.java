package com.anttoolkit.hbase.tasks.data.util.scan.filter;

import org.apache.hadoop.hbase.filter.*;
import org.apache.tools.ant.*;

import com.anttoolkit.general.tasks.*;

public class ColumnPaginationFilterConfig implements IFilterConfig
{
	private Integer limit;
	private Integer offset;

	public void setLimit(Integer limit)
	{
		this.limit = limit;
	}

	public void setOffset(Integer offset)
	{
		this.offset = offset;
	}

	@Override
	public Filter getFilter(GenericTask task)
	{
		validate(task);
		return new ColumnPaginationFilter(limit, offset);
	}

	@Override
	public void validate(GenericTask task)
	{
		if (limit == null)
		{
			throw new BuildException("Limit should be set");
		}

		if (offset == null)
		{
			throw new BuildException("Offset should be set");
		}
	}
}
