package com.anttoolkit.general.tasks.math;

import org.apache.tools.ant.*;

import com.anttoolkit.general.tasks.*;

public class CastToIntegerTask extends GenericTask
{
	private String value;
	private String property;

	public void setValue(String value)
	{
		this.value = value;
	}

	public void setProperty(String property)
	{
		this.property = property;
	}

	@Override
	public void doWork() throws BuildException
	{
		try
		{
			double val = Double.parseDouble(value);
			this.setPropertyThreadSafe(property, Long.toString((long)val));
		}
		catch (NumberFormatException e)
		{
			throw new BuildException("Incorrect number value specified: " + value);
		}
	}

	protected void validate()
	{
		if (value == null || value.trim().isEmpty())
		{
			throw new BuildException("Value should be specified");
		}

		if (property == null || property.trim().isEmpty())
		{
			throw new BuildException("Property name should be specified");
		}
	}
}
