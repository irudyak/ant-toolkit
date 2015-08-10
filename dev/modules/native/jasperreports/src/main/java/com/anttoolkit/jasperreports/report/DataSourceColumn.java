package com.anttoolkit.jasperreports.report;

import org.apache.tools.ant.*;

public class DataSourceColumn extends TypeAdapter
{
	private int position = -1;
	private String name;

	public void setName(String name)
	{
		this.name = name;
	}

	public String getName()
	{
		return name;
	}

	public void setPosition(int position)
	{
		if (position < 0)
		{
			throw new BuildException("Incorrect position specified: " + position);
		}

		this.position = position;
	}

	public int getPosition()
	{
		return position;
	}

	public void validate()
	{
		if (name == null)
		{
			throw new BuildException("Column name should be specified");
		}
	}

	public Object parse(String value)
	{
		return this.convertToPrimaryObject(value);
	}
}
