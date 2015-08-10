package com.anttoolkit.hbase.tasks.data.util.scan.filter;

import org.apache.hadoop.hbase.filter.*;
import org.apache.hadoop.hbase.util.*;
import org.apache.tools.ant.*;

import com.anttoolkit.general.tasks.*;

public class PrefixFilterConfig implements IFilterConfig
{
	private String prefix;

	public void setPrefix(String prefix)
	{
		this.prefix = prefix;
	}

	@Override
	public Filter getFilter(GenericTask task)
	{
		validate(task);
		return new PrefixFilter(Bytes.toBytes(prefix));
	}

	@Override
	public void validate(GenericTask task)
	{
		if (prefix == null)
		{
			throw new BuildException("Prefix should be specified");
		}
	}
}
