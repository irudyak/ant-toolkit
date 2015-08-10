package com.anttoolkit.hadoop.tasks.hdfs;

import java.io.*;

import org.apache.hadoop.fs.*;
import org.apache.tools.ant.*;

import com.anttoolkit.hadoop.tasks.hadoop.*;

public class TouchzTask extends GenericHadoopTask
{
	private String file;

	@Override
	protected void doHadoopWork() throws BuildException
	{
		try
		{
			getRemoteFileSystem().create(new Path(file)).close();
		}
		catch (IOException e)
		{
			throw new BuildException("Failed to create HDFS file: " + file, e);
		}
	}

	@Override
	protected void hadoopValidate()
	{
		if (file == null || file.trim().isEmpty())
		{
			throw new BuildException("File to create should be specified");
		}
	}
}
