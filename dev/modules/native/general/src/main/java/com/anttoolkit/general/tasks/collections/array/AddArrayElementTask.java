package com.anttoolkit.general.tasks.collections.array;

import org.apache.tools.ant.*;

import com.anttoolkit.general.tasks.*;
import com.anttoolkit.general.tasks.collections.array.util.*;

public class AddArrayElementTask
		extends GenericTask
{
	private String arrayName;
	private String value = null;

	public void setArray(String name)
	{
		arrayName = name;
	}

	public void setValue(String value)
	{
		this.value = value;
	}

	public void addText(String value)
	{
		this.value = value;
	}

	public void doWork()
			throws BuildException
	{
		ArrayManager.add(arrayName, value);
	}
}
