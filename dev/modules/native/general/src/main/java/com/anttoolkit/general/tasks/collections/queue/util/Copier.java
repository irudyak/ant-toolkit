package com.anttoolkit.general.tasks.collections.queue.util;

import java.util.*;

public class Copier  extends QueueProcessor<String, Void>
{
	@Override
	public Void processEntity(List<String> entity, String param)
	{
		List<String> dest = new LinkedList<String>();
		for (String value : entity)
		{
			dest.add(value);
		}

		QueueManager.init(param, dest, false);

		return null;
	}

	@Override
	public boolean readOnly()
	{
		return false;
	}
}
