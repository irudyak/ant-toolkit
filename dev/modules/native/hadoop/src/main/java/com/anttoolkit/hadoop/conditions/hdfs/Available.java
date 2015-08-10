package com.anttoolkit.hadoop.conditions.hdfs;

import java.io.*;

import org.apache.hadoop.fs.*;

import org.apache.tools.ant.*;

import com.anttoolkit.hadoop.tasks.hadoop.util.*;
import com.anttoolkit.hadoop.conditions.hadoop.*;

public class Available extends GenericHadoopCondition
{
	private static final String FILE_TYPE = "file";
	private static final String DIR_TYPE = "dir";

	private String file;
	private String type = FILE_TYPE;

	public void setFile(String file)
	{
		this.file = file;
	}

	public void setType(String type)
	{
		this.type = type.toLowerCase();
	}

	@Override
	public boolean evaluate() throws BuildException
	{
		try
		{
			FileStatus status = getRemoteFileSystem().getFileStatus(new Path(file));
			return status != null &&
					((status.isDirectory() && DIR_TYPE.equals(type)) ||
					(status.isFile() && FILE_TYPE.equals(type)));
		}
		catch (FileNotFoundException e)
		{
			return false;
		}
		catch (IOException e)
		{
			throw new BuildException("Failed to check status of the file: " + file, e);
		}
	}

	@Override
	protected void validate()
	{
		if (file == null || file.trim().isEmpty())
		{
			throw new BuildException("File name should be specified");
		}

		if (!FILE_TYPE.equals(file) && DIR_TYPE.equals(file))
		{
			throw new BuildException("Incorrect file type specified: " + type);
		}
	}
}
