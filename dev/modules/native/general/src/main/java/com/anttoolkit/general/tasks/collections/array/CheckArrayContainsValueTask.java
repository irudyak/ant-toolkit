package com.anttoolkit.general.tasks.collections.array;

import com.anttoolkit.general.tasks.collections.array.util.ArrayManager;
import com.anttoolkit.general.tasks.GenericTask;
import org.apache.tools.ant.*;

public class CheckArrayContainsValueTask
		extends GenericTask
{
	private String arrayName = null;
	private String value = null;
	private String property = null;

	public void setArray(String name)
	{
		arrayName = name;
	}

	public void setValue(String val)
	{
		value = val;
	}

	public void setProperty(String property)
	{
		this.property = property;
	}

	public void doWork() throws BuildException
	{
		boolean contains = ArrayManager.contains(arrayName, value);
		setPropertyThreadSafe(property, Boolean.toString(contains));
	}

	protected void validate()
	{
		if (arrayName == null)
		{
			throw new BuildException("Array name should be specified");
		}

		if (value == null)
		{
			throw new BuildException("Value to search for should be specified");
		}

		if (property == null)
		{
			throw new BuildException("Ant property name should be specified");
		}
	}
}
