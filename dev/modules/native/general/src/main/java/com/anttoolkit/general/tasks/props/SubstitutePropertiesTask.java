package com.anttoolkit.general.tasks.props;

import com.anttoolkit.general.tasks.GenericTask;
import org.apache.tools.ant.*;

public class SubstitutePropertiesTask extends GenericTask
{
	private String text = null;
	private String property = null;

	public void addText(String value)
	{
		text = getProject().replaceProperties(value);
	}

	public void setValue(String value)
	{
		this.text = value;
	}

	public void setProperty(String property)
	{
		this.property = property;
	}

	public void doWork() throws BuildException
	{
		String value = substituteProperties(this.getProject(), this.text);
		setPropertyThreadSafe(property, value);
	}

	protected void validate()
	{
		if (property == null)
		{
			throw new BuildException("Property name should be specified");
		}

		if (text == null)
		{
			throw new BuildException("Text or value should be specified");
		}
	}
}
