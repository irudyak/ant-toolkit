package com.anttoolkit.hadoop.tasks.hdfs;

import java.io.*;
import java.util.*;

import org.apache.hadoop.fs.*;
import org.apache.hadoop.io.*;
import org.apache.tools.ant.*;

import com.anttoolkit.general.tasks.collections.array.util.*;
import com.anttoolkit.hadoop.tasks.hadoop.*;
import com.anttoolkit.hadoop.tasks.hadoop.util.*;

public class MergeTask
		extends GenericHadoopTask
		implements FileProcessor
{
	private String dir;
	private String array;
	private String files;
	private String dest;
	private boolean overwrite = false;
	private String addString;

	private List<Path> paths = new ArrayList<Path>();

	public void setDir(String dir)
	{
		this.dir = dir;
	}

	public void addText(String files)
	{
		this.files = files;
	}

	public void setArray(String array)
	{
		this.array = array;
	}

	public void setDest(String dest)
	{
		this.dest = dest;
	}

	public void setAddString(String str)
	{
		addString = str;
	}

	public void setOverwrite(boolean overwrite)
	{
		this.overwrite = overwrite;
	}

	@Override
	public void doHadoopWork() throws BuildException
	{
		Path[] filesToMerge = getFilesToMerge();
		if (filesToMerge == null || filesToMerge.length == 0)
		{
			return;
		}

		OutputStream out = null;
		InputStream in = null;

		try
		{
			out = getRemoteFileSystem().create(new Path(dest), overwrite);

			int i = 0;

			for (Path path : filesToMerge)
			{
				in = getRemoteFileSystem().open(path);

				if (i != 0 && addString != null)
				{
					out.write(addString.getBytes("UTF-8"));
				}

				IOUtils.copyBytes(in, out, getRemoteFileSystem().getConf(), false);
				in.close();

				i = 1;
			}
		}
		catch (IOException e)
		{
			throw new BuildException("Files merge failed", e);
		}
		finally
		{
			IOUtils.closeStream(in);
			IOUtils.closeStream(out);
		}
	}

	@Override
	public void process(FileStatus status)
	{
		if (!status.isDirectory() && !paths.contains(status.getPath()))
		{
			paths.add(status.getPath());
		}
	}

	@Override
	protected void hadoopValidate()
	{
		if ((dir == null || dir.trim().isEmpty()) &&
			(array == null || array.trim().isEmpty()) &&
			(files == null || files.trim().isEmpty()))
		{
			throw new BuildException("Either dir or files or array should be specififed");
		}

		if (array != null && !ArrayManager.exists(array))
		{
			throw new BuildException("Array '" + array + "' doesn't exist");
		}

		if (dir != null && !dir.trim().isEmpty())
		{
			FileStatus status = getFileStatus(dir);
			if (status == null)
			{
				throw new IllegalArgumentException("Specified directory doesn't exist: " + dir);
			}

			if (status.isFile())
			{
				throw new IllegalArgumentException("Incorrect directory specified, cause actually it is a file: " + dir);
			}
		}

		FileStatus status = getFileStatus(dest);
		if (status != null)
		{
			if (!overwrite)
			{
				throw new BuildException("Incorrect destination specified, file with the same name already exists: " + dest);
			}
			else
			{
				try
				{
					getRemoteFileSystem().delete(new Path(dest), true);
				}
				catch (IOException e)
				{
					throw new BuildException("Failed to delete already existent destination file: " + dest);
				}
			}
		}
	}

	protected Path[] getFilesToMerge()
	{
		paths.clear();

		if (files != null)
		{
			String[] filesArray = files.split(",", -1);
			for (String file : filesArray)
			{
				checkFile(file.trim());

				Path path = new Path(file.trim());
				if (!paths.contains(path))
				{
					paths.add(path);
				}
			}
		}

		if (array != null)
		{
			int size = ArrayManager.size(array);
			for (int i = 0; i < size; i++)
			{
				String file = ArrayManager.get(array, i);
				checkFile(file);

				Path path = new Path(file);
				if (!paths.contains(path))
				{
					paths.add(path);
				}
			}
		}

		if (dir != null && !dir.trim().isEmpty())
		{
			processDirectoryFiles(this, getRemoteFileSystem(), dir, true);
		}

		return paths.toArray(new Path[0]);
	}

	private void checkFile(String file)
	{
		FileStatus status = getFileStatus(file);
		if (status == null)
		{
			throw new IllegalArgumentException("Specified file doesn't exist: " + file);
		}

		if (status.isDirectory())
		{
			throw new IllegalArgumentException("Incorrect file specified, cause actually it's a directory: " + file);
		}
	}
}
