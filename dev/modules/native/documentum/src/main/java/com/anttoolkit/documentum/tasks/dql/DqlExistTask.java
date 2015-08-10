package com.anttoolkit.documentum.tasks.dql;

import org.apache.tools.ant.*;

import com.documentum.fc.common.*;

import com.anttoolkit.documentum.common.*;

public class DqlExistTask
		extends GenericDocbaseTask
{
	private String dqlStatement = null;
	private String property = null;

	public void setQuery(String query)
	{
		dqlStatement = query;
	}

	public void addText(String text)
	{
		dqlStatement = getProject().replaceProperties(text);
	}

	public void setProperty(String property)
	{
		this.property = property;
	}

	public void doWork()
			throws BuildException
	{
		try
		{
			boolean exist = DqlHelper.exist(this.getSession(), dqlStatement);
			this.setPropertyThreadSafe(property, Boolean.toString(exist));
		}
		catch (DfException e)
		{
			throw new BuildException("Failed to execute DQL query to verify if anything exists", e);
		}
	}

	protected void validate()
	{
		if (dqlStatement == null)
		{
			throw new BuildException("DQL statement should be specified");
		}

		if (property == null)
		{
			throw new BuildException("Property should be specified");
		}
	}
}
