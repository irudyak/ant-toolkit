package com.anttoolkit.hadoop.tasks.hdfs;

import java.io.*;

import org.apache.hadoop.fs.*;
import org.apache.tools.ant.*;

import com.anttoolkit.hadoop.tasks.hadoop.*;

public class DeleteTask
		extends GenericHadoopTask
{
	private String file;

	public void setFile(String file)
	{
		this.file = file;
	}

	@Override
	public void doHadoopWork() throws BuildException
	{
		if (file == null || file.trim().isEmpty())
		{
			return;
		}

		FileStatus status = getFileStatus(file);
		if (status == null)
		{
			return;
		}

		try
		{
			getRemoteFileSystem().delete(new Path(file), true);
		}
		catch (FileNotFoundException e)
		{
		}
		catch (IOException e)
		{
			throw new BuildException("Failed to delete file: " + file);
		}
	}

	@Override
	protected void hadoopValidate()
	{
		if (file == null || file.trim().isEmpty())
		{
			throw new BuildException("File should be specified");
		}
	}
}
