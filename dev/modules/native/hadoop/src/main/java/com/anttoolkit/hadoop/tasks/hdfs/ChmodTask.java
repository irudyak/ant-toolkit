package com.anttoolkit.hadoop.tasks.hdfs;

import java.io.*;

import org.apache.hadoop.fs.*;
import org.apache.hadoop.fs.permission.*;

import org.apache.tools.ant.*;

import com.anttoolkit.hadoop.tasks.hadoop.*;
import com.anttoolkit.hadoop.tasks.hadoop.util.*;

public class ChmodTask
		extends GenericHadoopTask
		implements FileProcessor
{
	private String file;
	private ChmodParser parser;
	private boolean recursive = false;

	public void setPerm(String perm)
	{
		parser = new ChmodParser(perm);
	}

	public  void setRecursive(boolean recursive)
	{
		this.recursive = recursive;
	}

	public void setFile(String file)
	{
		this.file = file;
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

			getRemoteFileSystem().setPermission(new Path(file), new FsPermission(parser.applyNewPermission(status)));

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
			throw new BuildException("Failed to change permissions for the file: " + file, e);
		}
	}

	@Override
	public void process(FileStatus status)
	{
		try
		{
			getRemoteFileSystem().setPermission(status.getPath(), new FsPermission(parser.applyNewPermission(status)));
		}
		catch (FileNotFoundException e)
		{
			throw new BuildException("There are no file in a file system: " + status.getPath().toString(), e);
		}
		catch (IOException e)
		{
			throw new BuildException("Failed to change permissions for the file: " + status.getPath().toString(), e);
		}
	}

	@Override
	protected void hadoopValidate()
	{
		if (file == null || file.trim().isEmpty())
		{
			throw new BuildException("File should be specified");
		}

		if (parser == null)
		{
			throw new BuildException("Permissions should be specified");
		}
	}
}
