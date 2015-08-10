package com.anttoolkit.general.tasks.strings;

import org.apache.tools.ant.*;

import com.anttoolkit.general.tasks.*;

public class GetSubstringTask extends GenericTask
{
	private String string;
	private String property;
	private int start = -1;
	private int end = -1;

	public void setString(String string)
	{
		this.string = string;
	}

	public void setProperty(String property)
	{
		this.property = property;
	}

	public void setStart(int start)
	{
		this.start = start;
	}

	public void setEnd(int end)
	{
		this.end = end;
	}

	@Override
	public void doWork() throws BuildException
	{
		if (end < 0)
		{
			this.setPropertyThreadSafe(property, string.substring(start));
		}
		else
		{
			this.setPropertyThreadSafe(property, string.substring(start, end));
		}
	}

	@Override
	protected void validate()
	{
		if (string == null)
		{
			throw new BuildException("String from which to get substring should be specified");
		}

		if (property == null)
		{
			throw new BuildException("Property name should be specified");
		}

		if (start < 0)
		{
			throw new BuildException("Start index should be specified");
		}

		if (end < start)
		{
			throw new BuildException("End index can't be less that start index");
		}

		if (start > string.length() - 1)
		{
			throw new BuildException("Start index is bigger that length of the string");
		}

		if (end > string.length())
		{
			throw new BuildException("End index is bigger that length of the string");
		}
	}
}
