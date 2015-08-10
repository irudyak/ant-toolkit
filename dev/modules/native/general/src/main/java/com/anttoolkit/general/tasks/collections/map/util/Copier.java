package com.anttoolkit.general.tasks.collections.map.util;

import java.util.*;

public class Copier extends MapProcessor<String, Void>
{
	@Override
	public Void processEntity(Map<String, String> entity, String param)
	{
		Map<String, String> dest = new HashMap<String, String>();

		Set<String> keys = entity.keySet();
		for (String key : keys)
		{
			dest.put(key, entity.get(key));
		}

		MapManager.init(param, dest, false);

		return null;
	}

	@Override
	public boolean readOnly()
	{
		return false;
	}
}
