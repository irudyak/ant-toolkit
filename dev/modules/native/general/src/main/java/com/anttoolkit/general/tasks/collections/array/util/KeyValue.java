package com.anttoolkit.general.tasks.collections.array.util;

import java.util.*;

public class KeyValue implements Comparable
{
	private static class AscComparator implements Comparator<KeyValue>
	{
		@Override
		public int compare(KeyValue val1, KeyValue val2)
		{
			return val1.compareTo(val2);
		}
	}

	private static class DescComparator implements Comparator<KeyValue>
	{
		@Override
		public int compare(KeyValue val1, KeyValue val2)
		{
			return val2.compareTo(val1);
		}
	}

	public static final Comparator<KeyValue> ASC_COMPARATOR = new AscComparator();
	public static final Comparator<KeyValue> DESC_COMPARATOR = new DescComparator();

	public final String key;
	public final String val;

	public KeyValue(String key, String val)
	{
		this.key = key;
		this.val = val;
	}

	@Override
	public int compareTo(Object obj)
	{
		if (!(obj instanceof KeyValue))
		{
			throw new IllegalArgumentException("Incorrect object specified for comparison: " + obj);
		}

		try
		{
			Double arg1 = Double.parseDouble(val);
			Double arg2 = Double.parseDouble(((KeyValue)obj).val);

			return arg1.compareTo(arg2);
		}
		catch (NumberFormatException e) {}

		return val.compareTo(((KeyValue)obj).val);
	}
}
