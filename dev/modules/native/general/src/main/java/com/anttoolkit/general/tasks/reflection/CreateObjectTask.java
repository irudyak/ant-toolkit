package com.anttoolkit.general.tasks.reflection;

import java.util.*;

import org.apache.tools.ant.*;

import com.anttoolkit.general.tasks.*;
import com.anttoolkit.general.tasks.reflection.util.*;
import com.anttoolkit.general.common.*;

public class CreateObjectTask extends GenericTask
{
	private String className;
	private String reference;
	private List<Parameter> params = new LinkedList<Parameter>();

	public void setClass(String className)
	{
		this.className = className;
	}

	public void setReference(String reference)
	{
		this.reference = reference;
	}

	public void addConfiguredParameter(Parameter param)
	{
		params.add(param);
	}

	public void addConfiguredNullParameter(NullParameter param)
	{
		params.add(param);
	}

	@Override
	public void doWork() throws BuildException
	{
		Object[] paramValues = new Object[params.size()];
		Class[] paramTypes = new Class[params.size()];

		for (int i = 0; i < params.size(); i++)
		{
			Parameter param = params.get(i);
			paramValues[i] = param.getParamValue(this);
			paramTypes[i] = param.getParamClass(this);
		}

		try
		{
			if (!ReflectionHelper.hasConstructor(className, paramTypes))
			{
				throw new BuildException(className + " class doesn't have constructor necessary constructor: " + className + getConstructorSignature());
			}

			Object obj = ReflectionHelper.newInstance(className, paramValues, paramTypes);

			setReference(reference, obj);
		}
		catch (Exception e)
		{
			throw new BuildException("Failed to create new instance of " + className + " object", e);
		}
	}

	@Override
	protected void validate()
	{
		if (className == null || className.trim().isEmpty())
		{
			throw new BuildException("Class name should be specified");
		}

		if (reference == null || reference.trim().isEmpty())
		{
			throw new BuildException("Reference name should be specified");
		}
	}

	protected String getConstructorSignature()
	{
		StringBuilder builder = new StringBuilder();

		for (int i = 0; i < params.size(); i++)
		{
			Parameter param = params.get(i);

			if (builder.length() != 0)
			{
				builder.append(", ");
			}

			builder.append(param.getClass().getName()).append(" arg").append(i);
		}

		return "(" + builder.toString() + ")";
	}
}
