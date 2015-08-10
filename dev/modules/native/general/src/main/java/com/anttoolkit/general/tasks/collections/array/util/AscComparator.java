package com.anttoolkit.general.tasks.collections.array.util;

import java.util.Comparator;

public class AscComparator implements Comparator<String>
{
	public int compare(String val1, String val2)
	{
		try
		{
			Double arg1 = Double.parseDouble(val1);
			Double arg2 = Double.parseDouble(val2);

			return arg1.compareTo(arg2);
		}
		catch (NumberFormatException e) {}

		return val1.compareTo(val2);
	}
}
