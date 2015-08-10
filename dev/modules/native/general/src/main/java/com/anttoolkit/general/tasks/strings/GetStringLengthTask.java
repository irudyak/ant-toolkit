package com.anttoolkit.general.tasks.strings;

import org.apache.tools.ant.*;

import com.anttoolkit.general.tasks.*;

public class GetStringLengthTask extends GenericTask
{
	private String string;
	private String property;


	public void setString(String string)
	{
		this.string = string;
	}

	public void setProperty(String property)
	{
		this.property = property;
	}

	@Override
	public void doWork() throws BuildException
	{
		this.setPropertyThreadSafe(property, Integer.toString(string.length()));
	}

	protected void validate()
	{
		if (string == null)
		{
			throw new BuildException("String should be specified");
		}

		if (property == null || property.trim().isEmpty())
		{
			throw new BuildException("Property should be specified");
		}
	}
}
