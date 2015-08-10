package com.anttoolkit.sql.tasks;

import com.anttoolkit.general.tasks.GenericTask;
import org.apache.tools.ant.*;

import com.anttoolkit.sql.common.*;

public class SqlSetPropertyTask
		extends GenericTask
{
	private String sqlCommand = null;
	private String delimiter = ";";
	private String property = null;
	private String format = "MM/dd/yyyy hh:mm:ss";

	public void addText(String text)
	{
		sqlCommand = getProject().replaceProperties(text);
	}

	public void setDelimiter(String delimiter)
	{
		this.delimiter = delimiter;
	}

	public void setProperty(String name)
	{
		property = name;
	}

	public void setFormat(String format)
	{
		this.format = format;
	}

	public void doWork() throws BuildException
	{
		String value = SqlHelper.getStringParamFromFirstString(SqlSessionManager.getSession(), sqlCommand, 1, format, delimiter);
		this.setPropertyThreadSafe(property, value);
	}

	protected void validate()
	{
		if (sqlCommand == null || sqlCommand.trim().length() == 0)
		{
			throw new BuildException("SQL command is not specified");
		}

		if (property == null)
		{
			throw new BuildException("Property name is not specified");
		}
	}
}
