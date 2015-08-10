package com.anttoolkit.general.tasks.collections.array;

import org.apache.tools.ant.*;

import com.anttoolkit.general.tasks.*;
import com.anttoolkit.general.tasks.collections.array.util.*;

public class SetArrayElementTask
		extends GenericTask
{
	private String arrayName;
	private String value = null;
	private Integer index;

	public void setArray(String name)
	{
		arrayName = name;
	}

	public void setValue(String value)
	{
		this.value = value;
	}

	public void setIndex(int index)
	{
		this.index = index;
	}

	public void addText(String value)
	{
		this.value = value;
	}

	public void doWork()
			throws BuildException
	{
		ArrayManager.set(arrayName, index, value);
	}

	@Override
	protected void validate()
	{
		if (value == null)
		{
			throw new BuildException("Value should be specified");
		}

		if (index == null)
		{
			throw new BuildException("Index should be specified");
		}
	}
}
