package com.anttoolkit.general.tasks.file;

import java.io.*;
import java.util.regex.*;

import com.anttoolkit.general.tasks.GenericTask;
import com.anttoolkit.general.tasks.collections.array.util.ArrayManager;
import com.anttoolkit.general.tasks.file.util.FileTypeConst;
import org.apache.tools.ant.*;

public class GetDirectoryFilesTask
		extends GenericTask
{
	private String dir = null;
	private boolean recursive = false;
	private String fullNameArray = null;
	private String shortNameArray = null;
	private String nameArray = null;
	private String extensionArray = null;
	private String typeArray = null;
	private String filter = null;
	private String type = FileTypeConst.ALL_TYPE;
	private boolean cleanArrayIfExists = false;

	public void setDir(String dir)
	{
		this.dir = dir;
	}

	public void setRecursive(boolean recursive)
	{
		this.recursive = recursive;
	}

	public void setFullNameArray(String array)
	{
		fullNameArray = array;
	}

	public void setShortNameArray(String array)
	{
		shortNameArray = array;
	}

	public void setNameArray(String array)
	{
		nameArray = array;
	}

	public void setExtensionArray(String array)
	{
		extensionArray = array;
	}

	public void setTypeArray(String array)
	{
		typeArray = array;
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

	public void setFilter(String filter)
	{
		this.filter = filter;
	}

	public void setCleanArrayIfExists(boolean clean)
	{
		cleanArrayIfExists = clean;
	}

	public void doWork() throws BuildException
	{
		if (dir == null)
		{
			throw new BuildException("Directory should be specified");
		}

		File dir = new File(getFileFullPath(this.dir));
		if (!dir.isDirectory())
		{
			throw new BuildException("Invalid directory specified: " + this.dir);
		}

		if (fullNameArray == null && shortNameArray == null &&
			extensionArray == null && nameArray == null)
		{
			return;
		}

		if (cleanArrayIfExists)
		{
			if (fullNameArray != null)
			{
				ArrayManager.clear(fullNameArray, true);
			}

			if (shortNameArray != null)
			{
				ArrayManager.clear(shortNameArray, true);
			}

			if (nameArray != null)
			{
				ArrayManager.clear(nameArray, true);
			}

			if (extensionArray != null)
			{
				ArrayManager.clear(extensionArray, true);
			}

			if (typeArray != null)
			{
				ArrayManager.clear(typeArray, true);
			}
		}

		listDirectoryFiles(dir);
	}

	private void listDirectoryFiles(File dir)
	{
		File[] files = dir.listFiles();
		if (files == null)
		{
			return;
		}

		for (File file : files)
		{
			updateArrays(file);

			if (file.isDirectory() && recursive)
			{
				listDirectoryFiles(file);
			}
		}
	}

	private void updateArrays(File file)
	{
		String fileAbsolutePath = file.getAbsolutePath();
		if ((FileTypeConst.FILE_TYPE.equals(type) && file.isDirectory()) ||
			(FileTypeConst.DIR_TYPE.equals(type) && file.isFile()) ||
			(filter != null && !filter.trim().isEmpty() && !Pattern.matches(filter, fileAbsolutePath)))
		{
			return;
		}

		int index = fileAbsolutePath.lastIndexOf(File.separatorChar);
		String shortName = index <= 0 ? fileAbsolutePath : fileAbsolutePath.substring(index + 1);

		index = shortName.lastIndexOf(".");
		String name = index <= 0 ? shortName : shortName.substring(0, index);
		String extension = index <= 0 ? "" : shortName.substring(index + 1).toLowerCase();

		if (fullNameArray != null)
		{
			ArrayManager.add(fullNameArray, fileAbsolutePath);
		}

		if (shortNameArray != null)
		{
			ArrayManager.add(shortNameArray, shortName);
		}

		if (nameArray != null)
		{
			ArrayManager.add(nameArray, name);
		}

		if (extensionArray != null)
		{
			ArrayManager.add(extensionArray, extension);
		}

		if (typeArray != null)
		{
			ArrayManager.add(typeArray, file.isDirectory() ? FileTypeConst.DIR_TYPE : FileTypeConst.FILE_TYPE);
		}
	}
}
