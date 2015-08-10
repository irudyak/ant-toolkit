package com.anttoolkit.general.tasks.collections.array;

import com.anttoolkit.general.tasks.math.GenerateRandomIntegerTask;
import com.anttoolkit.general.tasks.GenericTask;
import com.anttoolkit.general.tasks.collections.array.util.ArrayManager;
import org.apache.tools.ant.*;

public class GetRandomArrayElementTask
		extends GenericTask
{
	private String arrayName = null;
	private String valueProperty = null;
	private String indexProperty = null;

	public void setArray(String name)
	{
		arrayName = name;
	}

	public void setValueProperty(String name)
	{
		valueProperty = name;
	}

	public void setIndexProperty(String name)
	{
		indexProperty = name;
	}

	public void doWork() throws BuildException
	{
		int size = ArrayManager.size(arrayName);
		int index = GenerateRandomIntegerTask.getRandomInt(0, size - 1);
		String value = ArrayManager.get(arrayName, index);

		if (valueProperty != null)
		{
			setPropertyThreadSafe(valueProperty, value);
		}

		if (indexProperty != null)
		{
			setPropertyThreadSafe(indexProperty, Integer.toString(index));
		}
	}

	protected void validate()
	{
		if (arrayName == null)
		{
			throw new BuildException("Array name should be specified");
		}

		if (valueProperty == null && indexProperty == null)
		{
			throw new BuildException("Value or index property should be specified");
		}
	}
}
