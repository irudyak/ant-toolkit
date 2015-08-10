package com.anttoolkit.general.tasks.collections.array;

import org.apache.tools.ant.*;

import com.anttoolkit.general.tasks.*;
import com.anttoolkit.general.tasks.collections.array.util.*;

public class SortArrayTask
		extends GenericTask
{
	private String array;
	private String keysArray;
	private String valuesArray;
	private boolean ascending = true;

	public void setArray(String name)
	{
		array = name;
	}

	public void setKeysArray(String name)
	{
		this.keysArray = name;
	}

	public void setValuesArray(String name)
	{
		this.valuesArray = name;
	}

	public void setAscending(boolean asc)
	{
		ascending = asc;
	}

	public void doWork() throws BuildException
	{
		if (array != null)
		{
			ArrayManager.sort(array, ascending);
		}

		if (keysArray != null && valuesArray != null)
		{
			ArrayManager.sort(keysArray, valuesArray, ascending);
		}
	}

	protected void validate()
	{
		if (array == null && keysArray == null && valuesArray == null)
		{
			throw new BuildException("No array specified to sort");
		}

		if (array != null && (keysArray != null || valuesArray != null))
		{
			throw new BuildException("Either array or key and values arrays should be specified");
		}

		if ((keysArray != null && valuesArray == null) ||
			(keysArray == null && valuesArray != null))
		{
			throw new BuildException("Both values and keys arrays should be specified");
		}

		if (keysArray != null && valuesArray != null &&
			ArrayManager.size(keysArray) != ArrayManager.size(valuesArray))
		{
			throw new BuildException("Sizes of keys and values arrays are not equal");
		}
	}
}