package com.anttoolkit.general.tasks.collections.array.util;

import java.util.*;

import org.apache.tools.ant.*;

public class Sorter extends ArrayProcessor<Object, Void>
{
	private static final Comparator<String> ASC_COMPARATOR = new AscComparator();
	private static final Comparator<String> DESC_COMPARATOR = new DescComparator();

	@Override
	public Void processEntity(List<String> entity, Object param)
	{
		if (param instanceof Boolean)
		{
			Collections.sort(entity, (Boolean)param ? ASC_COMPARATOR : DESC_COMPARATOR);
			return null;
		}

		if (!(param instanceof Object[]) || ((Object[])param).length != 2 ||
			!(((Object[])param)[0] instanceof Boolean) ||
			!(((Object[])param)[1] instanceof List))
		{
			throw new BuildException("Incorrect parameter specified: " + param);
		}

		Boolean ascending = (Boolean)(((Object[])param)[0]);
		List<String> values = (List<String>)((Object[])param)[1];

		List<KeyValue> keyVals = getKeyValuesList(entity, values);
		Collections.sort(keyVals, ascending ? KeyValue.ASC_COMPARATOR : KeyValue.DESC_COMPARATOR);

		entity.clear();
		values.clear();

		for (KeyValue keyVal : keyVals)
		{
			entity.add(keyVal.key);
			values.add(keyVal.val);
		}

		return null;
	}

	@Override
	public boolean readOnly()
	{
		return false;
	}

	private List<KeyValue> getKeyValuesList(List<String> keys, List<String> values)
	{
		List<KeyValue> result = new LinkedList<KeyValue>();

		int count = keys.size();
		for (int i = 0; i < count; i++)
		{
			result.add(new KeyValue(keys.get(i), values.get(i)));
		}

		return result;
	}
}
