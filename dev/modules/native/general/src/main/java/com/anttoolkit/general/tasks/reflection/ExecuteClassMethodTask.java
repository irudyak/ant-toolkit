package com.anttoolkit.general.tasks.reflection;

import org.apache.tools.ant.*;

import com.anttoolkit.general.common.*;

public class ExecuteClassMethodTask extends MethodInvocationTask
{
	private String className;

	public void setClass(String className)
	{
		this.className = className;
	}

	@Override
	protected Object getObject()
	{
		try
		{
			return ReflectionHelper.forName(className);
		}
		catch (ClassNotFoundException e)
		{
			throw new BuildException("Class " + className + " can't be found to invoke its method", e);
		}
	}

	@Override
	protected void validate()
	{
		super.validate();

		if (className == null || className.trim().isEmpty())
		{
			throw new BuildException("Class name should be specified");
		}
	}
}
