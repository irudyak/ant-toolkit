package com.anttoolkit.hadoop.tasks.hdfs;

import java.io.*;
import java.util.*;

import org.apache.hadoop.fs.*;
import org.apache.hadoop.fs.permission.*;
import org.apache.tools.ant.*;

import com.anttoolkit.hadoop.tasks.hadoop.*;
import com.anttoolkit.hadoop.tasks.hadoop.util.*;

public class RemoveAclTask
		extends GenericHadoopTask
		implements FileProcessor
{
	private String path;
	private boolean recursive = false;
	private boolean removeBase = false;
	private boolean removeDefault = false;
	private List<AclEntry> entries = null;

	public void setPath(String path)
	{
		this.path = path;
	}

	public  void setRecursive(boolean recursive)
	{
		this.recursive = recursive;
	}

	public void setRemoveBase(boolean remove)
	{
		this.removeBase = remove;
	}

	public void setRemoveDefault(boolean remove)
	{
		this.removeDefault = remove;
	}

	public void setAclSpec(String spec)
	{
		entries = AclEntry.parseAclSpec(spec, false);
	}

	@Override
	protected void doHadoopWork() throws BuildException
	{
		FileStatus status = getFileStatus(path);
		if (status == null)
		{
			throw new BuildException("There are no file in a file system: " + path);
		}

		process(status);

		if (recursive && status.isDirectory())
		{
			processDirectoryFiles(this, getRemoteFileSystem(), path, true);
		}
	}

	@Override
	protected void hadoopValidate()
	{
		if (path == null || path.trim().isEmpty())
		{
			throw new BuildException("Path should be specified");
		}

		if (!removeBase && !removeDefault && (entries == null || entries.isEmpty()))
		{
			throw new BuildException("Either ACL entries or removeBase or removeDefault flag should be specified");
		}
	}

	@Override
	public void process(FileStatus status)
	{
		try
		{
			if (removeBase)
			{
				getRemoteFileSystem().removeAcl(status.getPath());
			}

			if (removeDefault)
			{
				getRemoteFileSystem().removeDefaultAcl(status.getPath());
			}

			if (entries != null && !entries.isEmpty())
			{
				getRemoteFileSystem().removeAclEntries(status.getPath(), entries);
			}
		}
		catch (FileNotFoundException e)
		{
			throw new BuildException("There are no file in a file system: " + status.getPath().toString(), e);
		}
		catch (IOException e)
		{
			throw new BuildException("Failed change ACL for the file: " + status.getPath().toString(), e);
		}
	}
}
