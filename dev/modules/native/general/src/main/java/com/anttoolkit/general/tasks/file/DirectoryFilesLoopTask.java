package com.anttoolkit.general.tasks.file;

import java.io.*;
import java.util.*;
import java.util.regex.*;

import org.apache.tools.ant.*;

import com.anttoolkit.general.tasks.*;
import com.anttoolkit.general.tasks.file.util.*;

public class DirectoryFilesLoopTask
		extends GenericTask
		implements TaskContainer

{
	private String dir = null;
	private boolean recursive = false;
	private String fullNameProperty = null;
	private String shortNameProperty = null;
	private String nameProperty = null;
	private String extensionProperty = null;
	private String typeProperty = null;
	private String dirProperty = null;
	private String filter = null;
	private String type = FileTypeConst.ALL_TYPE;

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

	public void setDirProperty(String property)
	{
		this.dirProperty = property;
	}

	public void setFilter(String filter)
	{
		this.filter = filter;
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

		this.type = type.trim().toLowerCase();
	}

	@Override
	public void doWork() throws BuildException
	{
		if (dir == null)
		{
			throw new BuildException("Directory should be specified");
		}

		File dir = new File(getFileFullPath(this.dir));
		if (!dir.exists() || !dir.isDirectory())
		{
			throw new BuildException("Invalid directory specified: " + this.dir);
		}

		if (fullNameProperty == null && shortNameProperty == null &&
			nameProperty == null && extensionProperty == null && typeProperty == null)
		{
			return;
		}

		if (tasks.isEmpty())
		{
			return;
		}

		iterateThroughDirectoryFiles(dir);
	}

	@Override
	public void addTask(Task task)
	{
		tasks.add(task);
	}

	private void iterateThroughDirectoryFiles(File dir)
	{
		File[] files = dir.listFiles();
		if (files == null)
		{
			return;
		}

		for (File file : files)
		{
			if (updateProperties(file))
			{
				for (Task task : tasks)
				{
					task.perform();
				}
			}

			if (file.isDirectory() && recursive)
			{
				iterateThroughDirectoryFiles(file);
			}
		}
	}

	private boolean updateProperties(File file)
	{
		String fileAbsolutePath = file.getAbsolutePath();
		if ((FileTypeConst.FILE_TYPE.equals(type) && file.isDirectory()) ||
			(FileTypeConst.DIR_TYPE.equals(type) && file.isFile()) ||
			(filter != null && !filter.trim().isEmpty() && !Pattern.matches(filter, fileAbsolutePath)))
		{
			return false;
		}

		int index = fileAbsolutePath.lastIndexOf(File.separatorChar);
		String shortName = index <= 0 ? fileAbsolutePath : fileAbsolutePath.substring(index + 1);
		String parentDir = fileAbsolutePath.substring(0, index);

		index = shortName.lastIndexOf(".");
		String name = index <= 0 ? shortName : shortName.substring(0, index);
		String extension = index <= 0 ? "" : shortName.substring(index + 1).toLowerCase();

		if (fullNameProperty != null)
		{
			this.setPropertyThreadSafe(fullNameProperty, fileAbsolutePath);
		}

		if (dirProperty != null)
		{
			this.setPropertyThreadSafe(dirProperty, parentDir);
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
			this.setPropertyThreadSafe(typeProperty, file.isDirectory() ? FileTypeConst.DIR_TYPE : FileTypeConst.FILE_TYPE);
		}

		return true;
	}
}

