package com.anttoolkit.general.common;

public class NameValueHolder
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

	public void addText(String text)
	{
		value = text;
	}

	public String getName()
	{
		return name;
	}

	public String getValue()
	{
		return value;
	}
}
