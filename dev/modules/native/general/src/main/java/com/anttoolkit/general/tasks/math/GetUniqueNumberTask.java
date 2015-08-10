package com.anttoolkit.general.tasks.math;

import com.anttoolkit.general.tasks.GenericTask;
import org.apache.tools.ant.*;

public class GetUniqueNumberTask
		extends GenericTask
{
	private static long nextNumber = 0;

	private String property = null;

	public void setProperty(String property)
	{
		this.property = property;
	}

	public void doWork() throws BuildException
	{
		synchronized (GetUniqueNumberTask.class)
		{
			nextNumber++;
			setPropertyThreadSafe(property, Long.toString(nextNumber));
		}
	}

	protected void validate()
	{
		if (property == null)
		{
			throw new BuildException("Property name should be specified");
		}
	}
}
