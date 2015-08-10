package com.anttoolkit.documentum.tasks.dql;

import org.apache.tools.ant.*;

import com.anttoolkit.documentum.common.*;

public class DqlLoopPropertyTask
		extends GenericDocbaseTask
{
	private String property = null;
	private String columnName = null;
	private String format = null;
	private String nullValue = null;
	private String replaceSubstring = null;
	private String replaceWith = null;

	public void setProperty(String property)
	{
		this.property = property;
	}

	public void setColumn(String name)
	{
		columnName = name;
	}

	public void setFormat(String format)
	{
		this.format = format;
	}

	public void setNullValue(String value)
	{
		nullValue = value;
	}

	public void setReplaceSubstring(String value)
	{
		replaceSubstring = value;
	}

	public void setReplaceWith(String value)
	{
		replaceWith = value;
	}

	public void doWork()
			throws BuildException
	{
		String value = DqlLoopTask.getCurrentRowColumn(columnName, format);
		value = value == null ? nullValue : value;

		if (value != null && replaceSubstring != null && replaceWith != null)
		{
			value = value.replace(replaceSubstring, replaceWith);
		}

		this.setPropertyThreadSafe(property, value);
	}
}
