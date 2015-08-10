package com.anttoolkit.mongodb.common;

public class ValueHolder
{
	private String value;

	public void addText(String text)
	{
		value = text;
	}

	public String getValue()
	{
		return value;
	}
}
