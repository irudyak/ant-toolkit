package com.anttoolkit.general.tasks.collections.array.util;

import java.util.*;

public class Cleaner extends ArrayProcessor<Void,Void>
{
	@Override
	public Void processEntity(List<String> entity, Void param)
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
