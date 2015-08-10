package com.anttoolkit.general.tasks.collections.queue.util;

import java.util.*;

public class Extractor extends QueueProcessor<Void, String>
{
	@Override
	public String processEntity(List<String> entity, Void param)
	{
		return entity.isEmpty() ? null : entity.remove(0);
	}

	@Override
	public boolean readOnly()
	{
		return false;
	}
}
