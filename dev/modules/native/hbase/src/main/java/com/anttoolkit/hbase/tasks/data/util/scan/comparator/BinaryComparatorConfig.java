package com.anttoolkit.hbase.tasks.data.util.scan.comparator;

import org.apache.hadoop.hbase.filter.*;

import com.anttoolkit.general.tasks.*;

public class BinaryComparatorConfig extends ComparatorConfig
{
	@Override
	public ByteArrayComparable getComparator(GenericTask task)
	{
		validate(task);
		return new BinaryComparator(getValue(task));
	}
}
