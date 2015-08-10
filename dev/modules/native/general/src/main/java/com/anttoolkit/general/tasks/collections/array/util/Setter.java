package com.anttoolkit.general.tasks.collections.array.util;

import java.util.*;

import org.apache.tools.ant.*;

public class Setter extends ArrayProcessor<Object[], Void>
{
	@Override
	public Void processEntity(List<String> entity, Object[] param)
	{
		if (param == null || param.length != 2 ||
			!(param[0] instanceof Integer) || !(param[1] instanceof String))
		{
			throw new BuildException("Incorrect element specification to set array element");
		}

		Integer index = (Integer)param[0];
		String value = (String)param[1];

		entity.set(index, value);

		return null;
	}

	@Override
	public boolean readOnly()
	{
		return false;
	}
}
