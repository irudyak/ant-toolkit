package com.anttoolkit.general.tasks.file;

import com.anttoolkit.general.tasks.GenericTask;
import org.apache.tools.ant.*;

public class GetFileFullPathTask
	extends GenericTask
{
	private String file = null;
	private String property = null;

	public void setFile(String file)
	{
		this.file = file;
	}

	public void setProperty(String property)
	{
		this.property = property;
	}

	public void doWork() throws BuildException
	{
		setPropertyThreadSafe(property, getFileFullPath(file));
	}

	protected void validate()
	{
		if (file == null || file.trim().length() == 0)
		{
			throw new BuildException("File name should be specified");
		}

		if (property == null)
		{
			throw new BuildException("Property name should be specified");
		}
	}
}
