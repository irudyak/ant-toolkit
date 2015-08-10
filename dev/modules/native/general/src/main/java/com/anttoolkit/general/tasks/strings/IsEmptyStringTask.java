package com.anttoolkit.general.tasks.strings;

import com.anttoolkit.general.tasks.GenericTask;
import org.apache.tools.ant.*;

public class IsEmptyStringTask
		extends GenericTask
{
	private String value = null;
	private String property = null;

	public void setValue(String value)
	{
		this.value = value;
	}

	public void setProperty(String property)
	{
		this.property = property;
	}

	public void doWork() throws BuildException
	{
		boolean isEmpty = value == null || value.trim().length() == 0;
		setPropertyThreadSafe(property, Boolean.toString(isEmpty));
	}

	protected void validate()
	{
		if (property == null)
		{
			throw new BuildException("Property name should be specified");
		}
	}
}
