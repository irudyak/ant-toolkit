package com.anttoolkit.general.tasks.collections.array.util;

import java.util.*;

public class Copier extends ArrayProcessor<String, Void>
{
	@Override
	public Void processEntity(List<String> entity, String param)
	{
		List<String> dest = new ArrayList<String>(entity.size());
		for (String value : entity)
		{
			dest.add(value);
		}

		ArrayManager.init(param, dest, false);

		return null;
	}

	@Override
	public boolean readOnly()
	{
		return false;
	}
}
