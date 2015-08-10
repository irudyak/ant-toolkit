package com.anttoolkit.ldap.tasks;

import org.apache.tools.ant.*;

import com.anttoolkit.general.tasks.*;

import javax.naming.NamingException;
import javax.naming.directory.Attribute;

public class SearchAttributeValueTask
		extends GenericTask
{
	private String name;
	private int index = 0;
	private String property;
	private String defaultValue;

	public void setName(String name)
	{
		this.name = name;
	}

	public void setIndex(int index)
	{
		this.index = index;
	}

	public void setProperty(String property)
	{
		this.property = property;
	}

	public void setDefaultValue(String value)
	{
		this.defaultValue = value;
	}

	@Override
	public void doWork() throws BuildException
	{
		Attribute attr = SearchTask.getSearchResultAttribute(name);
		if (attr == null)
		{
			if (defaultValue != null)
			{
				this.setPropertyThreadSafe(property, defaultValue);
				return;
			}

			throw new BuildException("LDAP attribute '" + name + "' doesn't exist");
		}

		Object value;
		try
		{
			value = attr.get(index);
		}
		catch (NamingException e)
		{
			throw new BuildException("Failed to get value for LDAP attribute: " + name + "[" + index + "]", e);
		}

		this.setPropertyThreadSafe(property, value == null ? "" : value.toString());
	}

	@Override
	protected void validate()
	{
		if (name == null || name.trim().isEmpty())
		{
			throw new BuildException("LDAP attribute name should be specified");
		}

		if (property == null || property.trim().isEmpty())
		{
			throw new BuildException("Property to store LDAP attribute size should be specified");
		}

		if (index < 0)
		{
			throw new BuildException("Incorrect index '" + index + "' specified for LDAP attribute '" + name + "'");
		}
	}
}
