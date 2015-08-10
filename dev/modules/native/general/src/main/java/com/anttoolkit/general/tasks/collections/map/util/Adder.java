package com.anttoolkit.general.tasks.collections.map.util;

import java.util.*;
import java.util.Map.*;

public class Adder extends MapProcessor<Entry<String, String>, String>
{
	@Override
	public String processEntity(Map<String, String> entity, Entry<String, String> entry)
	{
		return entity.put(entry.getKey(), entry.getValue());
	}

	@Override
	public boolean readOnly()
	{
		return false;
	}
}
