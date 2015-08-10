package com.anttoolkit.hadoop.tasks.hdfs;

import java.util.regex.*;

import org.apache.hadoop.fs.*;
import org.apache.tools.ant.*;

import com.anttoolkit.general.tasks.collections.array.util.*;
import com.anttoolkit.general.tasks.file.util.*;
import com.anttoolkit.hadoop.tasks.hadoop.*;
import com.anttoolkit.hadoop.tasks.hadoop.util.*;

public class GetDirectoryFilesTask
		extends GenericHadoopTask
		implements FileProcessor
{
	private String dir = null;
	private boolean recursive = false;
	private String fullNameArray = null;
	private String shortNameArray = null;
	private String nameArray = null;
	private String extensionArray = null;
	private String typeArray = null;
	private String type = FileTypeConst.ALL_TYPE;
	private String filter = null;
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

		this.type = type;
	}

	public void setCleanArrayIfExists(boolean clean)
	{
		cleanArrayIfExists = clean;
	}

	@Override
	public void doHadoopWork() throws BuildException
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

		if (fullNameArray == null && shortNameArray == null &&
			extensionArray == null && nameArray == null)
		{
			return;
		}

		listDirectoryFiles(dir);
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

		if (fullNameArray != null)
		{
			ArrayManager.add(fullNameArray, absolutePath);
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
			ArrayManager.add(typeArray, status.isDirectory() ? FileTypeConst.DIR_TYPE : FileTypeConst.FILE_TYPE);
		}
	}

	private void listDirectoryFiles(String dir)
	{
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

		processDirectoryFiles(this, getRemoteFileSystem(), dir, recursive);
	}
}
