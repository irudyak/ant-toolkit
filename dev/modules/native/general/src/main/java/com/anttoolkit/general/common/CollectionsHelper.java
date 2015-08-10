package com.anttoolkit.general.common;

import java.util.*;

public class CollectionsHelper
{
	public static String toString(Collection coll)
	{
		return toString(coll, ",");
	}

	public static String toString(Collection coll, String separator)
	{
		if (coll == null || coll.isEmpty())
		{
			return "";
		}

		StringBuilder builder = new StringBuilder();

		for (Object obj : coll)
		{
			if (builder.length() != 0)
			{
				builder.append(separator);
			}

			String val = obj == null ? "" : obj.toString();

			builder.append(val == null ? "" : val);
		}

		return builder.toString();
	}

	public static List<String> asList(String value)
	{
		return asList(value, ",", false, false);
	}

	public static List<String> asList(String value, String separator)
	{
		return asList(value, separator, false, false);
	}

	public static List<String> asList(String value, String separator, boolean trimValues)
	{
		return asList(value, separator, trimValues, false);
	}

	public static List<String> asList(String value, String separator, boolean trimValues, boolean removeEmptyValues)
	{
		if (value == null)
		{
			return new LinkedList<String>();
		}

		String[] chunks = value.split(separator);
		ArrayList<String> result = new ArrayList<String>(chunks.length);

		for (String chunk : chunks)
		{
			String val = trimValues ? chunk.trim() : chunk;

			if (!removeEmptyValues || !val.isEmpty())
			{
				result.add(val);
			}
		}

		return result;
	}
}
