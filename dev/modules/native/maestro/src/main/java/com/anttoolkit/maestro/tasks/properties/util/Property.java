package com.anttoolkit.maestro.tasks.properties.util;

import org.apache.tools.ant.*;

public class Property
{
	private String name;
	private String value;

	public void setName(String name)
	{
		this.name = name;
	}

	public void setValue(String value)
	{
		this.value = value;
	}

	public void addText(String value)
	{
		this.value = value;
	}

	public String getName()
	{
		return name;
	}

	public String getValue()
	{
		return value;
	}

	public void validate()
	{
		if (name == null)
		{
			throw new BuildException("Property name is null");
		}

		if (value == null)
		{
			throw new BuildException("Value for the property " + name + " wasn't specified");
		}
	}
}
