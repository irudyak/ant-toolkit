package com.anttoolkit.hadoop.tasks.hdfs;

import java.io.*;

import org.apache.hadoop.fs.*;
import org.apache.tools.ant.*;

import com.anttoolkit.hadoop.tasks.hadoop.*;
import com.anttoolkit.hadoop.tasks.hadoop.util.*;

public class ChownTask
		extends GenericHadoopTask
		implements FileProcessor
{
	private String file;
	private String owner;
	private String group;
	private boolean recursive = false;

	public void setOwner(String owner)
	{
		this.owner = owner;
	}

	public void setGroup(String group)
	{
		this.group = group;
	}

	public void setFile(String file)
	{
		this.file = file;
	}

	public void setRecursive(boolean recursive)
	{
		this.recursive = recursive;
	}

	@Override
	public void doHadoopWork() throws BuildException
	{
		try
		{
			FileStatus status = getFileStatus(file);
			if (status == null)
			{
				throw new BuildException("There are no file in a file system: " + file);
			}

			owner = owner != null ? owner : status.getOwner();
			group = group != null ? group : status.getGroup();

			getRemoteFileSystem().setOwner(new Path(file), owner, group);

			if (recursive && status.isDirectory())
			{
				processDirectoryFiles(this, getRemoteFileSystem(), file, true);
			}
		}
		catch (FileNotFoundException e)
		{
			throw new BuildException("There are no file in a file system: " + file, e);
		}
		catch (IOException e)
		{
			throw new BuildException("Failed to change owner for the file: " + file, e);
		}
	}

	@Override
	public void process(FileStatus status)
	{
		try
		{
			getRemoteFileSystem().setOwner(status.getPath(), owner, group);
		}
		catch (FileNotFoundException e)
		{
		}
		catch (IOException e)
		{
			throw new BuildException("Failed to change owner for the file: " + status.getPath().toString(), e);
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
