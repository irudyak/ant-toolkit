package com.anttoolkit.general.tasks.collections.queue.util;

import java.util.*;

public class Cleaner extends QueueProcessor<String, Void>
{
	@Override
	public Void processEntity(List<String> entity, String param)
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
