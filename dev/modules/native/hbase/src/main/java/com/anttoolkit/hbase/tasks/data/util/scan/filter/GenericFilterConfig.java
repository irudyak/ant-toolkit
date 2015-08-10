package com.anttoolkit.hbase.tasks.data.util.scan.filter;

import org.apache.hadoop.hbase.filter.*;
import org.apache.tools.ant.*;

import com.anttoolkit.general.tasks.*;

public class GenericFilterConfig implements IFilterConfig
{
	private String ref;

	public void setRefid(String ref)
	{
		if (ref == null)
		{
			throw new BuildException("Can't set null reference for GenericFilter config");
		}
	}

	@Override
	public Filter getFilter(GenericTask task)
	{
		validate(task);
		return (Filter)task.getReference(ref);
	}

	@Override
	public void validate(GenericTask task)
	{
		if (ref == null || (task.getReference(ref) instanceof Filter))
		{
			throw new BuildException("Reference should be specified for GenericFilter config");
		}
	}
}
