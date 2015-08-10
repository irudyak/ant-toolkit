package com.anttoolkit.general.tasks.checksum;

import java.io.*;

import org.apache.tools.ant.*;

public class VerifyChecksumTask extends ChecksumTask
{
	private String str;
	private String file;
	private String checksum;
	private String checksumFile;
	private String property;

	public void setString(String str)
	{
		this.str = str;
	}

	public void setFile(String file)
	{
		this.file = file;
	}

	public void setChecksum(String checksum)
	{
		this.checksum = checksum;
	}

	public void setChecksumFile(String file)
	{
		this.checksumFile = file;
	}

	public void setProperty(String property)
	{
		this.property = property;
	}

	@Override
	public void doWork() throws BuildException
	{
		String _checksum = str != null ?
				generateChecksum(str) :
				generateChecksum(new File(getFileFullPath(file)));

		String checksum = this.checksum != null ?
				this.checksum :
				this.loadFileContent(checksumFile);

		this.setPropertyThreadSafe(property, Boolean.toString(_checksum.equals(checksum)));
	}

	@Override
	protected void validate()
	{
		if (str == null && file == null)
		{
			throw new BuildException("File or string for which to verify checksum should be specified");
		}

		if (str != null && file != null)
		{
			throw new BuildException("Either file or string for which to generate checksum should be specified");
		}

		if (!this.fileExists(file))
		{
			throw new BuildException("Specified file '" + file + "' doesn't exist");
		}

		if (checksum == null && checksumFile == null)
		{
			throw new BuildException("Checksum string or file should be specified");
		}

		if (checksum != null && checksumFile != null)
		{
			throw new BuildException("Either checksum string or file should be specified");
		}

		if (checksumFile != null && !this.fileExists(checksumFile))
		{
			throw new BuildException("Specified checksum file '" + checksumFile + "' doesn't exist");
		}

		if (property == null)
		{
			throw new BuildException("You should specify a property where to store boolean result of checksum verification");
		}
	}
}
