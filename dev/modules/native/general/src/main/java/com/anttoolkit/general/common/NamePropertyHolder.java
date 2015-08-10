package com.anttoolkit.general.common;

public class NamePropertyHolder
{
	private String name;
	private String property;

	public void setName(String name)
	{
		this.name = name;
	}

	public void setProperty(String property)
	{
		this.property = property;
	}

	public void addText(String text)
	{
		property = text;
	}

	public String getName()
	{
		return name;
	}

	public String getProperty()
	{
		return property;
	}
}
