package com.anttoolkit.general.tasks.refs;

import java.util.*;
import java.util.regex.*;

import org.apache.tools.ant.*;

import com.anttoolkit.general.tasks.*;
import com.anttoolkit.general.tasks.collections.array.util.*;

public class GetReferencesTask extends GenericTask
{
	private String array;
	private String filter;

	public void setArray(String array)
	{
		this.array = array;
	}

	public void setFilter(String filter)
	{
		this.filter = filter;
	}

	@Override
	public void doWork() throws BuildException
	{
		Set<String> keys = getProject().getReferences().keySet();
		for (String key : keys)
		{
			if (filter == null || Pattern.matches(filter, key))
			{
				ArrayManager.add(array, key);
			}
		}

		ArrayManager.sort(array, true);
	}

	protected void validate()
	{
		if (array == null || array.trim().isEmpty())
		{
			throw new BuildException("Array name should be specified");
		}

		if (!ArrayManager.exists(array))
		{
			throw new BuildException("Specified array '" + array + "' doesn't exist");
		}
	}
}
