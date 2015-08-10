package com.anttoolkit.hadoop.tasks.hdfs;

import java.io.*;

import org.apache.hadoop.fs.*;
import org.apache.tools.ant.*;

import com.anttoolkit.hadoop.tasks.hadoop.*;

public class SetReplicationTask
	extends GenericHadoopTask
{
	private String file;
	private short replication = 0;

	public void setFile(String file)
	{
		this.file = file;
	}

	public void setReplication(short replication)
	{
		this.replication = replication;
	}

	@Override
	public void doHadoopWork() throws BuildException
	{
		try
		{
			getRemoteFileSystem().setReplication(new Path(file), replication);
		}
		catch (IOException e)
		{
			throw new BuildException("Failed to set replication to " + replication + " for file: " + file, e);
		}
	}

	@Override
	protected void hadoopValidate()
	{
		if (file == null || file.trim().isEmpty())
		{
			throw new BuildException("File should be specified");
		}

		FileStatus status = getFileStatus(file);
		if (status == null)
		{
			throw new IllegalArgumentException("Specified file doesn't exist: " + file);
		}

		if (status.isDirectory())
		{
			throw new IllegalArgumentException("Incorrect file specified, cause actually it's a directory: " + file);
		}

		if (replication == 0)
		{
			throw new BuildException("Replication should be specified");
		}
	}
}
