package com.anttoolkit.general.tasks.collections.map.util;

import java.util.*;

public class Remover extends MapProcessor<String, String>
{
	@Override
	public String processEntity(Map<String, String> entity, String param)
	{
		return entity.remove(param);
	}

	@Override
	public boolean readOnly()
	{
		return false;
	}
}
