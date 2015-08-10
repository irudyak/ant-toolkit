package com.anttoolkit.sql.tasks;

import com.anttoolkit.general.tasks.GenericTask;
import org.apache.tools.ant.*;

public class SqlLoopPropertyTask
		extends GenericTask
{
	private String property = null;
	private String columnName = null;
	private String format = null;
	private String nullValue = null;
	private String replaceSubstring = null;
	private String replaceWith = null;

	public void setProperty(String name)
	{
		property = name;
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
		String value = SqlLoopTask.getCurrentRowColumn(columnName, format);
		value = value == null ? nullValue : value;

		if (value != null && replaceSubstring != null && replaceWith != null)
		{
			value = value.replace(replaceSubstring, replaceWith);
		}

		this.setPropertyThreadSafe(property, value);
	}
}
