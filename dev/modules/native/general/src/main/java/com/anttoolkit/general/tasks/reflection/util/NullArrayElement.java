package com.anttoolkit.general.tasks.reflection.util;

import com.anttoolkit.general.tasks.*;

public class NullArrayElement extends ArrayElement
{
	@Override
	public Object getValue(String clazz, GenericTask task)
	{
		return null;
	}
}
