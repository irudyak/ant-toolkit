package com.anttoolkit.general.tasks.collections.array.util;

import java.util.*;

public class ElementLookup extends ArrayProcessor<Integer, String>
{
	@Override
	public String processEntity(List<String> entity, Integer param)
	{
		return entity.get(param);
	}

	@Override
	public boolean readOnly()
	{
		return true;
	}
}
