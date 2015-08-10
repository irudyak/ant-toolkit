package com.anttoolkit.general.tasks.collections.array;

import org.apache.tools.ant.*;

import com.anttoolkit.general.tasks.*;
import com.anttoolkit.general.tasks.collections.array.util.*;

public class RemoveArrayElementTask
		extends GenericTask
{
	private String arrayName;
	private int index = -1;

	public void setArray(String name)
	{
		arrayName = name;
	}

	public void setIndex(int index)
	{
		this.index = index;
	}

	public void doWork()
			throws BuildException
	{
		ArrayManager.remove(arrayName, index);
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
	}
}
