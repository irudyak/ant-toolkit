package com.anttoolkit.hadoop.tasks.hdfs;

import java.util.*;
import java.util.regex.*;

import org.apache.hadoop.fs.*;
import org.apache.tools.ant.*;

import com.anttoolkit.hadoop.tasks.hadoop.*;
import com.anttoolkit.hadoop.tasks.hadoop.util.*;
import com.anttoolkit.general.tasks.file.util.*;

public class DirectoryFilesLoop
		extends GenericHadoopTask
		implements TaskContainer, FileProcessor
{
	private String dir = null;
	private boolean recursive = false;
	private String fullNameProperty = null;
	private String shortNameProperty = null;
	private String nameProperty = null;
	private String extensionProperty = null;
	private String typeProperty = null;
	private String statusReference = null;
	private String type = FileTypeConst.ALL_TYPE;
	private String filter = null;

	private List<Task> tasks = new LinkedList<Task>();

	public void setDir(String dir)
	{
		this.dir = dir;
	}

	public void setRecursive(boolean recursive)
	{
		this.recursive = recursive;
	}

	public void setFullNameProperty(String property)
	{
		fullNameProperty = property;
	}

	public void setShortNameProperty(String property)
	{
		shortNameProperty = property;
	}

	public void setNameProperty(String property)
	{
		nameProperty = property;
	}

	public void setExtensionProperty(String property)
	{
		extensionProperty = property;
	}

	public void setTypeProperty(String property)
	{
		typeProperty = property;
	}

	public void setStatusReference(String reference)
	{
		statusReference = reference;
	}

	public void setType(String type)
	{
		if (type == null)
		{
			throw new IllegalArgumentException("Type can't be null");
		}

		if (!FileTypeConst.DIR_TYPE.equals(type.toLowerCase().trim()) &&
			!FileTypeConst.FILE_TYPE.equals(type.toLowerCase().trim()) &&
			!FileTypeConst.ALL_TYPE.equals(type.toLowerCase().trim()))
		{
			throw new IllegalArgumentException("Illegal file type specified: " + type);
		}

		this.type = type;
	}

	public void setFilter(String filter)
	{
		this.filter = filter;
	}

	@Override
	public void process(FileStatus status)
	{
		String absolutePath = getRemoteFilePath(status.getPath());
		if ((FileTypeConst.FILE_TYPE.equals(type) && status.isDirectory()) ||
			(FileTypeConst.DIR_TYPE.equals(type) && !status.isDirectory()) ||
			(filter != null && !filter.trim().isEmpty() && !Pattern.matches(filter, absolutePath)))
		{
			return;
		}

		String shortName = status.getPath().getName();

		int index = shortName.lastIndexOf(".");
		String name = index <= 0 ? shortName : shortName.substring(0, index);
		String extension = index <= 0 ? "" : shortName.substring(index + 1).toLowerCase();

		if (fullNameProperty != null)
		{
			this.setPropertyThreadSafe(fullNameProperty, absolutePath);
		}

		if (shortNameProperty != null)
		{
			this.setPropertyThreadSafe(shortNameProperty, shortName);
		}

		if (nameProperty != null)
		{
			this.setPropertyThreadSafe(nameProperty, name);
		}

		if (extensionProperty != null)
		{
			this.setPropertyThreadSafe(extensionProperty, extension);
		}

		if (typeProperty != null)
		{
			this.setPropertyThreadSafe(typeProperty, status.isDirectory() ? FileTypeConst.DIR_TYPE : FileTypeConst.FILE_TYPE);
		}

		if (statusReference != null)
		{
			this.setReference(statusReference, status);
		}

		for (Task task : tasks)
		{
			task.perform();
		}
	}

	@Override
	public void doHadoopWork() throws BuildException
	{
		if (fullNameProperty == null && shortNameProperty == null &&
			nameProperty == null && extensionProperty == null)
		{
			return;
		}

		if (tasks.isEmpty())
		{
			return;
		}

		processDirectoryFiles(this, getRemoteFileSystem(), dir, recursive);
	}

	@Override
	public void addTask(Task task)
	{
		tasks.add(task);
	}

	@Override
	protected void hadoopValidate()
	{
		if (dir == null)
		{
			throw new BuildException("Directory should be specified");
		}

		FileStatus status = getFileStatus(dir);
		if (status == null)
		{
			throw new BuildException("Directory doesn't exist: " + dir);
		}

		if (!status.isDirectory())
		{
			throw new BuildException("Invalid directory specified: " + dir);
		}
	}

}
