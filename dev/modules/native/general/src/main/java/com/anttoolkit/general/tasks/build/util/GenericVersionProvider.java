package com.anttoolkit.general.tasks.build.util;

import java.lang.reflect.*;
import java.util.*;

import com.anttoolkit.general.tasks.GenericTask;
import org.apache.tools.ant.*;

import com.anttoolkit.general.common.*;

public abstract class GenericVersionProvider
		implements IBuildVersionProvider
{
	@Override
	public void init(GenericTask task, Map<String, String> properties)
	{
		Field[] fields = ReflectionHelper.getDeclaredFields(this.getClass(), null);
		for (Field field: fields)
		{
			if (!field.isAnnotationPresent(VersionProviderParam.class))
			{
				continue;
			}

			VersionProviderParam param = field.getAnnotation(VersionProviderParam.class);
			if (!properties.containsKey(param.value()))
			{
				continue;
			}

			//make private and protected fields also accessible
			field.setAccessible(true);

			try
			{
				field.set(this, properties.get(param.value()));
			}
			catch (IllegalAccessException e)
			{
				throw new BuildException("Failed to set build version provider param '" + param.value() + "'", e);
			}
		}
	}

	@Override
	public void validate(GenericTask task)
	{
	}
}
