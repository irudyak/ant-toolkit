package com.anttoolkit.general.tasks.collections.map.util;

import java.util.*;

public class Cleaner extends MapProcessor<Void, Void>
{
	@Override
	public Void processEntity(Map<String, String> entity, Void param)
	{
		entity.clear();
		return null;
	}

	@Override
	public boolean readOnly()
	{
		return false;
	}
}
