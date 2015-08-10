package com.anttoolkit.hadoop.conditions.hdfs;

import java.io.*;

import org.apache.hadoop.fs.*;
import org.apache.tools.ant.*;

import com.anttoolkit.hadoop.conditions.hadoop.*;

public class IsEmpty extends GenericHadoopCondition
{
	private String path;

	@Override
	protected boolean evaluate() throws BuildException
	{
		FileStatus status;

		try
		{
			status = getRemoteFileSystem().getFileStatus(new Path(path));
		}
		catch (FileNotFoundException e)
		{
			return true;
		}
		catch (IOException e)
		{
			throw new BuildException("Failed to check status of the file: " + path, e);
		}

		return status.getLen() == 0;
	}

	@Override
	protected void validate()
	{
		if (path == null || path.trim().isEmpty())
		{
			throw new BuildException("HDFS path should be specified");
		}
	}
}
