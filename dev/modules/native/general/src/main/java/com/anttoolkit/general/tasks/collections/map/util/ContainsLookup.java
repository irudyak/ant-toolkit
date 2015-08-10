package com.anttoolkit.general.tasks.collections.map.util;

import java.util.*;

public class ContainsLookup extends MapProcessor<String, Boolean>
{
	@Override
	public Boolean processEntity(Map<String, String> entity, String param)
	{
		return entity.containsKey(param);
	}

	@Override
	public boolean readOnly()
	{
		return true;
	}
}
