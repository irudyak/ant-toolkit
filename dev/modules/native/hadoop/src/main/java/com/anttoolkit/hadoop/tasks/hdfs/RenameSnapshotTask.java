package com.anttoolkit.hadoop.tasks.hdfs;

import java.io.*;

import org.apache.hadoop.fs.*;
import org.apache.hadoop.hdfs.*;
import org.apache.tools.ant.*;

import com.anttoolkit.hadoop.tasks.hadoop.*;

public class RenameSnapshotTask extends GenericHadoopTask
{
	private String path;
	private String oldName;
	private String newName;

	public void setPath(String path)
	{
		this.path = path;
	}

	public void setOldName(String oldName)
	{
		this.oldName = oldName;
	}

	public void setNewName(String newName)
	{
		this.newName = newName;
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
			fs.renameSnapshot(new Path(path), oldName, newName);
		}
		catch (IOException e)
		{
			throw new BuildException("Failed to rename snapshot '" + oldName + "' to '" + newName + "' for directory: " + path, e);
		}
	}

	@Override
	protected void hadoopValidate()
	{
		if (path == null || path.trim().length() == 0)
		{
			throw new BuildException("Directory for which to delete snapshot should be specified");
		}

		if (oldName == null || oldName.trim().length() == 0)
		{
			throw new BuildException("Snapshot old name should be specified");
		}

		if (newName == null || newName.trim().length() == 0)
		{
			throw new BuildException("Snapshot new name should be specified");
		}
	}
}
