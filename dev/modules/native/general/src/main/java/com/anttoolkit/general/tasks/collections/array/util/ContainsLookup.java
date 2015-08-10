package com.anttoolkit.general.tasks.collections.array.util;

import java.util.*;

public class ContainsLookup extends ArrayProcessor<String, Boolean>
{
	@Override
	public Boolean processEntity(List<String> entity, String param)
	{
		return entity.contains(param);
	}

	@Override
	public boolean readOnly()
	{
		return true;
	}
}
