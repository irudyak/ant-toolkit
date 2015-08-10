package com.anttoolkit.general.tasks.strings;

import com.anttoolkit.general.tasks.GenericTask;
import org.apache.tools.ant.*;

public class ToLowercaseTask
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
		setPropertyThreadSafe(property, value.toLowerCase());
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
