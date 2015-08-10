package com.anttoolkit.general.tasks.concurrent;

import com.anttoolkit.general.tasks.GenericTask;
import org.apache.tools.ant.*;

import com.anttoolkit.general.tasks.concurrent.util.*;

public class GetThreadAssociatedValueTask
		extends GenericTask
{
	private int index = 0;
	private String property = null;

	public void setIndex(int index)
	{
		this.index = index;
	}

	public void setProperty(String property)
	{
		this.property = property;
	}

	public void doWork() throws BuildException
	{
		if (property == null)
		{
			throw new BuildException("Property should be specified");
		}

		Thread thread = Thread.currentThread();

		if (!(thread instanceof TasksThread))
		{
			throw new BuildException("Thread '" + thread.getName() + "' should be the instance of " + TasksThread.class.getName());
		}

		String[] values = ((TasksThread)thread).getAssociatedValues();
		if (values == null || values.length == 0)
		{
			throw new BuildException("There are no values associated with thread: " + thread.getName());
		}

		if (index < 0 || index >= values.length)
		{
			throw new BuildException("Incorrect index " + index + " for value associated with thread: " + thread.getName());
		}

		setPropertyThreadSafe(property, values[index]);
	}
}
