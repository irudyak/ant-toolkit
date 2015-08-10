package com.anttoolkit.ldap.tasks;

import javax.naming.directory.*;

import org.apache.tools.ant.*;

import com.anttoolkit.general.tasks.*;

public class SearchAttributeSizeTask
		extends GenericTask
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

	@Override
	public void doWork() throws BuildException
	{
		Attribute attr = SearchTask.getSearchResultAttribute(name);
		this.setPropertyThreadSafe(property, attr == null ? Integer.toString(0) : Integer.toString(attr.size()));
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
	}
}
