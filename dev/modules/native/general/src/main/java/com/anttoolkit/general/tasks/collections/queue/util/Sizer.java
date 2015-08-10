package com.anttoolkit.general.tasks.collections.queue.util;

import java.util.*;

public class Sizer extends QueueProcessor<Void, Integer>
{
	@Override
	public Integer processEntity(List<String> entity, Void param)
	{
		return entity.size();
	}

	@Override
	public boolean readOnly()
	{
		return true;
	}
}
