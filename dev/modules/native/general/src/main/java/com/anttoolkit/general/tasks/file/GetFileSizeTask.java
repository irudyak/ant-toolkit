package com.anttoolkit.general.tasks.file;

import java.io.*;

import org.apache.tools.ant.*;

import com.anttoolkit.general.tasks.*;

public class GetFileSizeTask extends GenericTask
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
		File fileObj = new File(getFileFullPath(file));
		this.setPropertyThreadSafe(property, Long.toString(fileObj.length()));
	}

	protected void validate()
	{
		if (file == null || file.trim().isEmpty())
		{
			throw new BuildException("File name should be specified");
		}

		if (!fileExists(file))
		{
			throw new BuildException("Specified file doesn't exist");
		}

		if (property == null || property.trim().isEmpty())
		{
			throw new BuildException("Property to hold file size should be specified");
		}
	}
}
