package com.anttoolkit.sql.tasks;

import com.anttoolkit.general.tasks.GenericTask;
import org.apache.tools.ant.*;

import com.anttoolkit.sql.common.*;

public class SqlExistTask
		extends GenericTask
{
	private String query = null;
	private String delimiter = ";";
	private String property = null;

	public void setQuery(String query)
	{
		this.query = query;
	}

	public void setDelimiter(String delimiter)
	{
		this.delimiter = delimiter;
	}

	public void addText(String text)
	{
		query = getProject().replaceProperties(text);
	}

	public void setProperty(String property)
	{
		this.property = property;
	}

	public void doWork() throws BuildException
	{
		boolean exist = SqlHelper.exist(SqlSessionManager.getSession(), query, delimiter);
		this.setPropertyThreadSafe(property, Boolean.toString(exist));
	}

	protected void validate()
	{
		if (query == null)
		{
			throw new BuildException("SQL query should be specified");
		}

		if (property == null)
		{
			throw new BuildException("Property should be specified");
		}
	}
}
