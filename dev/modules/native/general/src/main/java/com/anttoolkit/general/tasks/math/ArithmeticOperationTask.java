package com.anttoolkit.general.tasks.math;

import com.anttoolkit.general.tasks.GenericTask;
import org.apache.tools.ant.*;

import com.anttoolkit.general.tasks.math.operation.*;

public abstract class ArithmeticOperationTask
		extends GenericTask
{
	private String arg1;
	private String arg2;
	private String property;

	public void setArg1(String arg)
	{
		arg1 = arg;
	}

	public void setArg2(String arg)
	{
		arg2 = arg;
	}

	public void setProperty(String property)
	{
		this.property = property;
	}

	protected abstract IMathOperation getOperation();

	public void doWork()
			throws BuildException
	{
		validate();
		this.setPropertyThreadSafe(property, getOperation().execute(arg1, arg2));
	}

	protected void validate()
	{
		if (arg1 == null)
		{
			throw new BuildException("arg1 is not specified");
		}

		if (arg2 == null)
		{
			throw new BuildException("arg2 is not specified");
		}

		if (property == null)
		{
			throw new BuildException("Result property is not specified");
		}
	}
}
