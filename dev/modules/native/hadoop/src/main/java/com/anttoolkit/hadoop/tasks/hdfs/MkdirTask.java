package com.anttoolkit.hadoop.tasks.hdfs;

import java.io.*;

import org.apache.hadoop.fs.*;
import org.apache.tools.ant.*;

import com.anttoolkit.hadoop.tasks.hadoop.*;

public class MkdirTask
		extends GenericHadoopTask
{
	private String dir;

	public void setDir(String dir)
	{
		this.dir = dir;
	}

	@Override
	public void doHadoopWork() throws BuildException
	{
		try
		{
			getRemoteFileSystem().mkdirs(new Path(dir));
		}
		catch (IOException e)
		{
			throw new BuildException("Failed to create directory: " + dir, e);
		}
	}

	@Override
	protected void hadoopValidate()
	{
		if (dir == null || dir.trim().isEmpty())
		{
			throw new BuildException("Directory name should be specified");
		}
	}
}
