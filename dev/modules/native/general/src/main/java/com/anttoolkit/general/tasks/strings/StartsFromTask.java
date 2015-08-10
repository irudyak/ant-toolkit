package com.anttoolkit.general.tasks.strings;

import com.anttoolkit.general.tasks.GenericTask;
import org.apache.tools.ant.*;

public class StartsFromTask
		extends GenericTask
{
	private String value = null;
	private String prefix = null;
	private String property = null;
	private boolean ignoreCase = false;

	public void setValue(String value)
	{
		this.value = value;
	}

	public void setPrefix(String prefix)
	{
		this.prefix = prefix;
	}

	public void setProperty(String property)
	{
		this.property = property;
	}

	public void setIgnoreCase(boolean ignoreCase)
	{
		this.ignoreCase = ignoreCase;
	}

	public void doWork() throws BuildException
	{
		boolean startsWith = value != null && (ignoreCase ? value.toLowerCase().trim().startsWith(prefix.toLowerCase()) : value.trim().startsWith(prefix));
		setPropertyThreadSafe(property, Boolean.toString(startsWith));
	}

	protected void validate()
	{
		if (property == null)
		{
			throw new BuildException("Property name is not specified");
		}

		if (prefix == null)
		{
			throw new BuildException("Prefix is not specified");
		}

		if (value == null)
		{
			throw new BuildException("Value is not specified");
		}
	}
}
