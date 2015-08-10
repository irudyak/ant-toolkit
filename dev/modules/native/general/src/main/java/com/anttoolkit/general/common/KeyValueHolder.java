package com.anttoolkit.general.common;

public class KeyValueHolder
{
	private String key;
	private String value;

	public void setKey(String key)
	{
		this.key = key;
	}

	public void setValue(String value)
	{
		this.value = value;
	}

	public void addText(String text)
	{
		value = text;
	}

	public String getKey()
	{
		return key;
	}

	public String getValue()
	{
		return value;
	}
}
