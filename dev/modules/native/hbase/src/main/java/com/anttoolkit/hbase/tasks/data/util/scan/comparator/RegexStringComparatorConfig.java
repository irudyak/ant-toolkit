package com.anttoolkit.hbase.tasks.data.util.scan.comparator;

import org.apache.hadoop.hbase.filter.*;
import org.apache.hadoop.hbase.util.*;

import com.anttoolkit.general.tasks.*;

public class RegexStringComparatorConfig extends ComparatorConfig
{
	@Override
	public ByteArrayComparable getComparator(GenericTask task)
	{
		validate(task);
		return new RegexStringComparator(Bytes.toString(getValue(task)));
	}
}
