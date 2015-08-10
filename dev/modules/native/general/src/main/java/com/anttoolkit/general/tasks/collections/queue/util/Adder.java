package com.anttoolkit.general.tasks.collections.queue.util;

import java.util.*;

public class Adder extends QueueProcessor<String, Void>
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
