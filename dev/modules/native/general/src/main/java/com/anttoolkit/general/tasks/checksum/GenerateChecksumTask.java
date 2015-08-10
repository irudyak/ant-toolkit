package com.anttoolkit.general.tasks.checksum;

import java.io.*;

import org.apache.tools.ant.*;

public class GenerateChecksumTask extends ChecksumTask
{
	private String str;
	private String file;
	private String checksumProperty;
	private String checksumFile;

	public void setString(String str)
	{
		this.str = str;
	}

	public void setFile(String file)
	{
		this.file = file;
	}

	public void setProperty(String property)
	{
		this.checksumProperty = property;
	}

	public void setChecksumFile(String file)
	{
		this.checksumFile = file;
	}

	@Override
	public void doWork() throws BuildException
	{
		String checksum = str != null ?
				generateChecksum(str) :
				generateChecksum(new File(getFileFullPath(file)));

		if (checksumProperty != null)
		{
			this.setPropertyThreadSafe(checksumProperty, checksum);
		}

		if (checksumFile != null)
		{
			this.saveContentToFile(checksumFile, checksum);
		}
	}

	@Override
	protected void validate()
	{
		if (str == null && file == null)
		{
			throw new BuildException("File or string for which to generate checksum should be specified");
		}

		if (str != null && file != null)
		{
			throw new BuildException("Either file or string for which to generate checksum should be specified");
		}

		if (!this.fileExists(file))
		{
			throw new BuildException("Specified file '" + file + "' doesn't exist");
		}

		if (checksumProperty == null && checksumFile == null)
		{
			throw new BuildException("Checksum property and/or file should be specified");
		}
	}
}
