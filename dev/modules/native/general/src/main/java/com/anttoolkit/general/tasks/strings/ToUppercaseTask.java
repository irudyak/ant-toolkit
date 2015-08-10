package com.anttoolkit.general.tasks.strings;

import org.apache.tools.ant.*;

import com.anttoolkit.general.tasks.*;

public class ToUppercaseTask
		extends GenericTask
{
	private String value = null;
	private String property = null;

	public void setValue(String value)
	{
		this.value = value;
	}

	public void setProperty(String property)
	{
		this.property = property;
	}

	public void doWork() throws BuildException
	{
		setPropertyThreadSafe(property, value.toUpperCase());
	}

	protected void validate()
	{
		if (property == null)
		{
			throw new BuildException("Property name is not specified");
		}

		if (value == null)
		{
			throw new BuildException("Value is not specified");
		}
	}
}
