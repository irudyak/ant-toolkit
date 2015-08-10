package com.anttoolkit.hadoop.types;

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

	public String getName()
	{
		return name;
	}

	public String getValue()
	{
		return value;
	}

	@Override
	public boolean equals(Object obj)
	{
		if (!(obj instanceof Property))
		{
			return false;
		}

		Property prop = (Property)obj;

		return compareNames(prop) && compareValues(prop);
	}

	private boolean compareNames(Property prop)
	{
		return (prop.name == null && name == null) ||
				(prop.name != null && name != null && prop.name.equals(name));
	}

	private boolean compareValues(Property prop)
	{
		return (prop.value == null && value == null) ||
				(prop.value != null && value != null && prop.value.equals(value));
	}
}
