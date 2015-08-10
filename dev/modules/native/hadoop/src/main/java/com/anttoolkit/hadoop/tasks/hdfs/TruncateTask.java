package com.anttoolkit.hadoop.tasks.hdfs;

import java.io.*;

import org.apache.hadoop.fs.*;
import org.apache.tools.ant.*;

import com.anttoolkit.hadoop.tasks.hadoop.*;

public class TruncateTask extends GenericHadoopTask
{
	private String path;
	private long length = -1;

	public void setPath(String path)
	{
		this.path = path;
	}

	public void setLength(long length)
	{
		this.length = length;
	}

	@Override
	protected void doHadoopWork() throws BuildException
	{
		Path filePath = new Path(path);

		try
		{
			getRemoteFileSystem().truncate(filePath, length);
		}
		catch (IOException e)
		{
			throw new BuildException("Failed to truncate file: " + path, e);
		}

		for(;;)
		{
	        if(getFileStatus(path).getLen() == length)
			{
				break;
			}

	        try
			{
				Thread.sleep(1000);
			}
			catch (InterruptedException ignored) {}
	    }
	}

	@Override
	protected void hadoopValidate()
	{
		if (path == null || path.trim().length() == 0)
		{
			throw new BuildException("Path to truncate should be specified");
		}

		if (length == -1)
		{
			throw new BuildException("Length to truncate to, should be specified");
		}

		if (length < 0)
		{
			throw new BuildException("Length must be >= 0");
		}
	}
}
