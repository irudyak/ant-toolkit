package com.anttoolkit.general.tasks.collections.array;

import com.anttoolkit.general.tasks.GenericTask;
import com.anttoolkit.general.tasks.collections.array.util.ArrayManager;

import org.apache.tools.ant.*;

public class GetArrayElementTask
		extends GenericTask
{
	private String arrayName;
	private int index = -1;
	private String property;

	public void setArray(String name)
	{
		arrayName = name;
	}

	public void setIndex(int index)
	{
		this.index = index;
	}

	public void setProperty(String name)
	{
		property = name;
	}

	public void doWork()
			throws BuildException
	{
		this.setPropertyThreadSafe(property, ArrayManager.get(arrayName, index));
	}

	protected void validate()
	{
		if (arrayName == null || arrayName.trim().length() == 0)
		{
			throw new BuildException("Array name doesn't specified");	
		}

		if (index == -1)
		{
			throw new BuildException("Array index doesn't specified");	
		}

		if (property == null || property.trim().length() == 0)
		{
			throw new BuildException("Property name doesn't specified");	
		}
	}
}
