package com.anttoolkit.hadoop.tasks.hdfs;

import java.io.*;

import org.apache.hadoop.fs.*;
import org.apache.hadoop.hdfs.*;
import org.apache.tools.ant.*;

import com.anttoolkit.hadoop.tasks.hadoop.*;

public class AllowSnapshotTask extends GenericHadoopTask
{
	private String path;

	public void setPath(String path)
	{
		this.path = path;
	}

	@Override
	protected void doHadoopWork() throws BuildException
	{
		FileSystem fs = getRemoteFileSystem();
		if (!(fs instanceof DistributedFileSystem))
		{
			throw new IllegalArgumentException("FileSystem " + fs.getUri() + " is not an HDFS file system");
		}

		try
		{
			((DistributedFileSystem)fs).allowSnapshot(new Path(path));
		}
		catch (IOException e)
		{
			throw new BuildException("Failed to allow snapshot for path: " + path, e);
		}
	}

	@Override
	protected void hadoopValidate()
	{
		if (path == null || path.trim().length() == 0)
		{
			throw new BuildException("Directory for which to allow snapshot should be specified");
		}
	}
}
