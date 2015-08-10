package com.anttoolkit.general.tasks.collections.array.util;

import java.util.List;

public class Adder extends ArrayProcessor<String, Void>
{
	@Override
	public Void processEntity(List<String> entity, String param)
	{
		entity.add(param);
		return null;
	}

	@Override
	public boolean readOnly()
	{
		return false;
	}
}
