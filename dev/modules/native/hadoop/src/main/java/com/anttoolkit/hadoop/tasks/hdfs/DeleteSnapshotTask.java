package com.anttoolkit.hadoop.tasks.hdfs;

import java.io.*;

import org.apache.hadoop.fs.*;
import org.apache.hadoop.hdfs.*;
import org.apache.tools.ant.*;

import com.anttoolkit.hadoop.tasks.hadoop.*;

public class DeleteSnapshotTask extends GenericHadoopTask
{
	private String path;
	private String snapshot;

	public void setPath(String path)
	{
		this.path = path;
	}

	public void setSnapshot(String snapshot)
	{
		this.snapshot = snapshot;
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
			fs.deleteSnapshot(new Path(path), snapshot);
		}
		catch (IOException e)
		{
			throw new BuildException("Failed to delete snapshot '" + snapshot + "' for directory: " + path, e);
		}
	}

	@Override
	protected void hadoopValidate()
	{
		if (path == null || path.trim().length() == 0)
		{
			throw new BuildException("Directory for which to delete snapshot should be specified");
		}

		if (snapshot == null || snapshot.trim().length() == 0)
		{
			throw new BuildException("Snapshot name should be specified");
		}
	}
}
