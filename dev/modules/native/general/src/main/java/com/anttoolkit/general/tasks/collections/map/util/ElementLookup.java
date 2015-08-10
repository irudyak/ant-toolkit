package com.anttoolkit.general.tasks.collections.map.util;

import java.util.Map;

public class ElementLookup extends MapProcessor<String, String>
{
	@Override
	public String processEntity(Map<String, String> entity, String param)
	{
		return entity.get(param);
	}

	@Override
	public boolean readOnly()
	{
		return true;
	}
}
