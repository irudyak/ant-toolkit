package com.anttoolkit.general.common;

public class ValueHolder
{
	private String value;

	public void setValue(String value)
	{
		this.value = value;
	}

	public void addText(String text)
	{
		value = text;
	}

	public String getValue()
	{
		return value;
	}
}
