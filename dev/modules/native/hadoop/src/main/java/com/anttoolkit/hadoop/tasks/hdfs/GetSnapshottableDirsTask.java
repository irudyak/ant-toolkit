package com.anttoolkit.hadoop.tasks.hdfs;

import java.io.*;

import org.apache.hadoop.fs.*;
import org.apache.hadoop.hdfs.*;
import org.apache.hadoop.hdfs.protocol.*;
import org.apache.tools.ant.*;

import com.anttoolkit.hadoop.tasks.hadoop.*;
import com.anttoolkit.general.tasks.collections.array.util.*;

public class GetSnapshottableDirsTask extends GenericHadoopTask
{
	private String array;

	public void setArray(String array)
	{
		this.array = array;
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
			SnapshottableDirectoryStatus[] statuses = ((DistributedFileSystem)fs).getSnapshottableDirListing();
			if (statuses == null || statuses.length == 0)
			{
				return;
			}

			for (SnapshottableDirectoryStatus status : statuses)
			{
				ArrayManager.add(array, this.getRemoteFilePath(status.getFullPath()));
			}
		}
		catch (IOException e)
		{
			throw new BuildException("Failed to get snapshottable dirs", e);
		}
	}

	@Override
	protected void hadoopValidate()
	{
		if (array == null || array.trim().length() == 0)
		{
			throw new BuildException("Array for snapshottable dirs should be specified");
		}

		if (!ArrayManager.exists(array))
		{
			throw new BuildException("Specified array '" + array + "' doesn't exist");
		}
	}
}
