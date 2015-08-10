package com.anttoolkit.general.tasks.strings;

import com.anttoolkit.general.tasks.GenericTask;
import org.apache.tools.ant.*;

public class ReplaceTextSubstringTask
		extends GenericTask
{
	private String textToReplace;
	private String substring;
	private String replaceWith;
	private String property;

	public void setString(String value)
	{
		textToReplace = value;
	}

	public void addText(String value)
	{
		textToReplace = getProject().replaceProperties(value);
	}

	public void setSubstring(String substring)
	{
		this.substring = substring;
	}

	public void setReplaceWith(String value)
	{
		replaceWith = value;
	}

	public void setProperty(String property)
	{
		this.property = property;
	}

	public void doWork() throws BuildException
	{
		validate();

		String value = textToReplace.replace(substring, replaceWith);
		this.setPropertyThreadSafe(property, value);
	}

	protected void validate()
	{
		if (textToReplace == null)
		{
			throw new BuildException("Text value is not specified");
		}

		if (substring == null)
		{
			throw new BuildException("Substring is not specified");
		}

		if (replaceWith == null)
		{
			throw new BuildException("Text to replace with is not specified");
		}

		if (property == null)
		{
			throw new BuildException("Property name is not specified");
		}
	}
}
