package com.anttoolkit.ldap.tasks;

import javax.naming.*;
import javax.naming.directory.*;

import org.apache.tools.ant.*;

public class LdapSetPropertyTask
		extends GenericSearchTask
{
	private String attribute;
	private int index = 0;
	private String property;
	private String defaultValue;

	public void setAttribute(String attribute)
	{
		this.attribute = attribute;
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
		NamingEnumeration<SearchResult> result = search();
		Attribute attr;

		try
		{
			if (!result.hasMore())
			{
				if (defaultValue != null)
				{
					this.setPropertyThreadSafe(property, defaultValue);
					return;
				}

				throw new BuildException("There are no result returned by LDAP search");
			}

			attr = result.next().getAttributes().get(attribute);
		}
		catch (NamingException e)
		{
			throw new BuildException("Failed to get results returned by LDAP search", e);
		}

		Object value;
		try
		{
			if (attr != null)
			{
				value = attr.get(index);
			}
			else if (defaultValue != null)
			{
				value = defaultValue;
			}
			else
			{
				throw new BuildException("Result returned by LDAP search doesn't contain '" + attribute + "' attribute");
			}
		}
		catch (NamingException e)
		{
			throw new BuildException("Failed to get value for LDAP attribute: " + attribute + "[" + index + "]", e);
		}

		this.setPropertyThreadSafe(property, value == null ? "" : value.toString());
	}

	@Override
	protected void validate()
	{
		if (attribute == null || attribute.trim().isEmpty())
		{
			throw new BuildException("LDAP attribute name should be specified");
		}

		if (property == null || property.trim().isEmpty())
		{
			throw new BuildException("Property to store LDAP attribute size should be specified");
		}

		if (index < 0)
		{
			throw new BuildException("Incorrect index '" + index + "' specified for LDAP attribute '" + attribute + "'");
		}
	}
}
