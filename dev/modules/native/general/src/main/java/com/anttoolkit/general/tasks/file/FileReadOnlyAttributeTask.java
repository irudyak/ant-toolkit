package com.anttoolkit.general.tasks.file;

import java.io.*;

import com.anttoolkit.general.tasks.GenericTask;
import org.apache.tools.ant.*;

public class FileReadOnlyAttributeTask
		extends GenericTask
{
	private boolean readOnly = false;
	private String file = null;
	private String dir = null;


	public void setReadOnly(boolean readOnly)
	{
		this.readOnly = readOnly;
	}

	public void setFile(String file)
	{
		this.file = file;
	}

	public void setDir(String dir)
	{
		this.dir = dir;
	}

	@Override
	public void doWork() throws BuildException
	{
		if (file != null)
		{
			setFileReadOnlyAttr(new File(getFileFullPath(file)));
		}

		if (dir != null)
		{
			setDirectoryFilesReadOnlyAttr(new File(getFileFullPath(dir)));
		}
	}

	protected void validate()
	{
		if (file == null && dir == null)
		{
			throw new BuildException("Either file or dir should be specified");
		}
	}

	private void setFileReadOnlyAttr(File file)
	{
		if (file != null && !file.isDirectory())
		{
			file.setWritable(!readOnly);
		}
	}

	private void setDirectoryFilesReadOnlyAttr(File dir)
	{
		if (!dir.isDirectory())
		{
			throw new BuildException("Incorrect directory name specified: " + dir);
		}

		File[] files = dir.listFiles();
		for (File file : files)
		{
			if (file.isDirectory())
			{
				setDirectoryFilesReadOnlyAttr(file);
			}
			else
			{
				setFileReadOnlyAttr(file);
			}
		}
	}
}
