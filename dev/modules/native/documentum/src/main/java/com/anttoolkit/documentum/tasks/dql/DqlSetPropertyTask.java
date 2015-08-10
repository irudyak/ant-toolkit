package com.anttoolkit.documentum.tasks.dql;

import org.apache.tools.ant.*;

import com.anttoolkit.documentum.common.*;

import com.documentum.fc.client.*;
import com.documentum.fc.common.*;

public class DqlSetPropertyTask
		extends GenericDocbaseTask
{
	private String dqlStatement = null;
	private String property = null;
	private String format = IDfTime.DF_TIME_PATTERN18;

	public void addText(String text)
	{
		dqlStatement = getProject().replaceProperties(text);
	}

	public void setProperty(String name)
	{
		property = name;
	}

	public void setFormat(String format)
	{
		this.format = format;
	}

	public void doWork()
			throws BuildException
	{
		try
		{
			IDfTypedObject obj = DqlHelper.getFirstString(this.getSession(), dqlStatement);
			String attrName = obj.getAttr(0).getName();
			String value = DocbaseObjectsHelper.getAttributeValueAsString(obj, attrName, format);

			this.setPropertyThreadSafe(property, value);
		}
		catch (DfException e)
		{
			throw new BuildException("Failed to execute DQL query to set property", e);
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
			throw new BuildException("propName attribute should be specified");
		}
	}
}
