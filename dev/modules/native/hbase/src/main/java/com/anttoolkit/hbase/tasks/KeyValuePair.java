package com.anttoolkit.hbase.tasks;

public class KeyValuePair
{
	private String key;
	private String value;

	public void validate()
	{
		if (key == null || key.trim().isEmpty())
		{
			throw new IllegalStateException("Key/name in a KeyValuePair can't be null");
		}
	}

	public void setKey(String key)
	{
		this.key = key;
	}

	public void setName(String name)
	{
		key = name;
	}

	public void setValue(String value)
	{
		this.value = value;
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
