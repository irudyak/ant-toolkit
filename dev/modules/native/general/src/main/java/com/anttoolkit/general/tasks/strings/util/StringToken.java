package com.anttoolkit.general.tasks.strings.util;

import org.apache.tools.ant.*;

public class StringToken
{
	private int index = -1;
	private String property;
	private String defaultValue;

	public void setIndex(int index)
	{
		this.index = index;
	}

	public void setProperty(String property)
	{
		this.property = property;
	}

	public void setDefault(String value)
	{
		this.defaultValue = value;
	}

	public int getIndex()
	{
		return index;
	}

	public String getProperty()
	{
		return property;
	}

	public String getDefault()
	{
		return defaultValue;
	}

	public void validate()
	{
		if (index == -1)
		{
			throw new BuildException("Token index should be specified");
		}

		if (property == null)
		{
			throw new BuildException("Property to store token value should be specified");
		}
	}
}
