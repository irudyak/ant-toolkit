package com.anttoolkit.general.tasks.collections.array;

import com.anttoolkit.general.tasks.collections.array.util.ArrayManager;
import com.anttoolkit.general.tasks.GenericTask;
import org.apache.tools.ant.*;

public class GetArraySizeTask
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
		int size = ArrayManager.size(arrayName);
		this.setPropertyThreadSafe(property, Integer.toString(size));
	}

	protected void validate()
	{
		if (arrayName == null)
		{
			throw new BuildException("Array name should be specified");
		}

		if (property == null)
		{
			throw new BuildException("Property name should be specified");
		}
	}
}