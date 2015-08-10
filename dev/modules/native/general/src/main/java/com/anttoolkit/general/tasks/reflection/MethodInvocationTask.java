package com.anttoolkit.general.tasks.reflection;

import java.util.*;

import org.apache.tools.ant.*;

import com.anttoolkit.general.common.*;
import com.anttoolkit.general.tasks.*;
import com.anttoolkit.general.tasks.reflection.util.*;

public abstract class MethodInvocationTask extends GenericTask
{
	private String method;
	private List<Parameter> params = new LinkedList<Parameter>();
	private String property;
	private String reference;

	public void setMethod(String method)
	{
		this.method = method;
	}

	public void setResultProperty(String property)
	{
		this.property = property;
	}

	public void setResultReference(String reference)
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

		Object obj = getObject();

		try
		{
			boolean returnsVoid = ReflectionHelper.checkMethodReturnsVoid(obj, method, paramTypes);
			Object result = ReflectionHelper.invokeMethod(obj, method, paramValues, paramTypes);

			if (returnsVoid)
			{
				return;
			}

			if (property != null)
			{
				setPropertyThreadSafe(property, result == null ? null : result.toString());
			}

			if (reference != null)
			{
				setReference(reference, result);
			}
		}
		catch (NoSuchMethodException e)
		{
			throw new BuildException("Method " + method + getMethodSignature() + " doesn't exist", e);
		}
		catch (Exception e)
		{
			throw new BuildException("Failed to invoke method " + method + getMethodSignature(), e);
		}
	}

	protected abstract Object getObject();

	@Override
	protected void validate()
	{
		if (method == null || method.trim().isEmpty())
		{
			throw new BuildException("Method name should be specified");
		}
	}

	protected String getMethodSignature()
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
