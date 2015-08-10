package com.anttoolkit.hadoop.tasks.hdfs;

import java.io.*;

import org.apache.hadoop.fs.*;
import org.apache.hadoop.util.*;
import org.apache.tools.ant.*;

import com.anttoolkit.hadoop.tasks.hadoop.*;

public class ChecksumTask extends GenericHadoopTask
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
	protected void doHadoopWork() throws BuildException
	{
		if (getFileStatus(file) == null)
		{
			throw new BuildException("Specified file doesn't exist: " + file);
		}

		FileChecksum checksum;

		try
		{
			checksum = getRemoteFileSystem().getFileChecksum(new Path(file));
		}
		catch (IOException e)
		{
			throw new BuildException("Failed to get file checksum: " + file, e);
		}

		String checksumStr = StringUtils.byteToHexString(checksum.getBytes(), 0, checksum.getLength());

		this.setPropertyThreadSafe(property, checksum.getAlgorithmName() + "," + checksumStr);
	}

	@Override
	protected void hadoopValidate()
	{
		if (file == null)
		{
			throw new BuildException("File should be specified");
		}

		if (property == null)
		{
			throw new BuildException("Ant property to hold file checksum should be specified");
		}
	}
}
