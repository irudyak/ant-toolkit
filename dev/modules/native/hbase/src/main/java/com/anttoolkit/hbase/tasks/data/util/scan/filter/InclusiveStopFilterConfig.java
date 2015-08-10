package com.anttoolkit.hbase.tasks.data.util.scan.filter;

import org.apache.hadoop.hbase.filter.*;
import org.apache.hadoop.hbase.util.*;
import org.apache.tools.ant.*;

import com.anttoolkit.general.tasks.*;

public class InclusiveStopFilterConfig implements IFilterConfig
{
	private String stopRowKey;

	public void setStopRowKey(String stopRowKey)
	{
		this.stopRowKey = stopRowKey;
	}

	@Override
	public Filter getFilter(GenericTask task)
	{
		validate(task);
		return new InclusiveStopFilter(Bytes.toBytes(stopRowKey));
	}

	@Override
	public void validate(GenericTask task)
	{
		if (stopRowKey == null)
		{
			throw new BuildException("Stop rowkey should be specified");
		}
	}
}
