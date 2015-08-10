package com.anttoolkit.general.tasks.collections.map.util;

import java.util.Map;

public class Sizer extends MapProcessor<Void, Integer>
{
	@Override
	public Integer processEntity(Map<String, String> entity, Void param)
	{
		return entity.size();
	}

	@Override
	public boolean readOnly()
	{
		return true;
	}
}
