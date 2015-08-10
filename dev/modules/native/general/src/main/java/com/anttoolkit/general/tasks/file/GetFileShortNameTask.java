package com.anttoolkit.general.tasks.file;

import org.apache.tools.ant.*;

import com.anttoolkit.general.tasks.*;

public class GetFileShortNameTask extends GenericTask
{
	private String file;
	private String property;

	public void setFile(String file)
	{
		this.file = file;
	}

	public void setProperty(String property)
	{
		this.property = property;
	}

	@Override
	public void doWork() throws BuildException
	{
		String shortName = file.trim().replace("\\", "/");
		int index = shortName.lastIndexOf("/");
		shortName = index == -1 ? shortName : shortName.substring(index + 1);
		this.setPropertyThreadSafe(property, shortName);
	}

	protected void validate()
	{
		if (file == null)
		{
			throw new BuildException("File name should be specified");
		}

		if (property == null)
		{
			throw new BuildException("Property name should be specified");
		}
	}
}
