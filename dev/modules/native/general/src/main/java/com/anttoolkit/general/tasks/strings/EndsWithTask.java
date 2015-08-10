package com.anttoolkit.general.tasks.strings;

import com.anttoolkit.general.tasks.GenericTask;
import org.apache.tools.ant.*;

public class EndsWithTask
		extends GenericTask
{
	private String value = null;
	private String postfix = null;
	private String property = null;
	private boolean ignoreCase = false;

	public void setValue(String value)
	{
		this.value = value;
	}

	public void setPostfix(String postfix)
	{
		this.postfix = postfix;
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
		boolean endsWith = value != null && (ignoreCase ? value.toLowerCase().trim().endsWith(postfix.toLowerCase()) : value.trim().endsWith(postfix));
		setPropertyThreadSafe(property, Boolean.toString(endsWith));
	}

	protected void validate()
	{
		if (property == null)
		{
			throw new BuildException("Property name is not specified");
		}

		if (postfix == null)
		{
			throw new BuildException("Postfix is not specified");
		}

		if (value == null)
		{
			throw new BuildException("Value is not specified");
		}
	}
}
