package com.anttoolkit.general.tasks.collections.array.util;

import org.apache.tools.ant.*;

import java.util.*;

public class Remover extends ArrayProcessor<Integer, Void>
{
	@Override
	public Void processEntity(List<String> entity, Integer param)
	{
		if (param >= entity.size() || param < 0)
		{
			throw new BuildException("Specified index '" + param + "' of the element to remove from array is out of it`s range");
		}

		entity.remove(param.intValue());
		return null;
	}

	@Override
	public boolean readOnly()
	{
		return false;
	}
}
