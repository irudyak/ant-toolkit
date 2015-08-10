package com.anttoolkit.general.tasks.collections.array;

import org.apache.tools.ant.*;

import com.anttoolkit.general.tasks.*;
import com.anttoolkit.general.tasks.collections.array.util.*;

public class CheckArrayExistsTask
		extends GenericTask
{
	private String arrayName = null;
	private String property = null;

	public void setArray(String name)
	{
		arrayName = name;
	}

	public void setProperty(String property)
	{
		this.property = property;
	}

	public void doWork() throws BuildException
	{
		setPropertyThreadSafe(property, Boolean.toString(ArrayManager.exists(arrayName)));
	}

	protected void validate()
	{
		if (arrayName == null)
		{
			throw new BuildException("Array name should be specified");
		}

		if (property == null)
		{
			throw new BuildException("Ant property name should be specified");
		}
	}
}
