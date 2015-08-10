package com.anttoolkit.general.tasks.collections.array;

import com.anttoolkit.general.tasks.GenericTask;
import com.anttoolkit.general.tasks.collections.array.util.ArrayManager;

import org.apache.tools.ant.*;

public class ClearArrayTask
		extends GenericTask
{
	private String arrayName = null;

	public void setArray(String name)
	{
		arrayName = name;
	}

	public void doWork() throws BuildException
	{
		if (arrayName == null)
		{
			throw new BuildException("Array name should be specified");
		}

		ArrayManager.clear(arrayName, false);
	}
}
