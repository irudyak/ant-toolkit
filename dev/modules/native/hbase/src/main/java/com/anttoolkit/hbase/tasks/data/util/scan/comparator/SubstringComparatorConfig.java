package com.anttoolkit.hbase.tasks.data.util.scan.comparator;

import org.apache.hadoop.hbase.filter.*;
import org.apache.hadoop.hbase.util.*;

import com.anttoolkit.general.tasks.*;

public class SubstringComparatorConfig extends ComparatorConfig
{
	@Override
	public ByteArrayComparable getComparator(GenericTask task)
	{
		validate(task);
		return new SubstringComparator(Bytes.toString(getValue(task)));
	}
}
