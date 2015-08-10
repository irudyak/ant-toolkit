package com.anttoolkit.hbase.tasks.data.util.scan.comparator;

import org.apache.hadoop.hbase.filter.*;

import com.anttoolkit.general.tasks.*;

public class NullComparatorConfig extends ComparatorConfig
{
	@Override
	public ByteArrayComparable getComparator(GenericTask task)
	{
		return new NullComparator();
	}

	public void validate(GenericTask task)
	{
	}
}
