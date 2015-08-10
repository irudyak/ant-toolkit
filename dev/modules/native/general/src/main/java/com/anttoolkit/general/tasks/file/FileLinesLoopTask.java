package com.anttoolkit.general.tasks.file;

import java.io.*;
import java.util.*;

import com.anttoolkit.general.tasks.GenericTask;
import org.apache.tools.ant.*;

public class FileLinesLoopTask
		extends GenericTask
		implements TaskContainer
{
	private String file = null;
	private String iterationProperty = null;
	private String numberProperty = null;
	private String encoding = null;
	private long startFromLine = 0;
	private boolean trim = false;
	private boolean ignoreEmptyLines = false;

	private List<Task> tasks = new LinkedList<Task>();

	public void addTask(Task task)
	{
		tasks.add(task);
	}

	public void setFile(String file)
	{
		this.file = file;
	}

	public void setEncoding(String encoding)
	{
		this.encoding = encoding;
	}

	public void setIterationProperty(String property)
	{
		this.iterationProperty = property;
	}

	public void setNumberProperty(String property)
	{
		this.numberProperty = property;
	}

	public void setStartFromLine(long startFromLine)
	{
		this.startFromLine = startFromLine;
	}

	public void setTrim(boolean trim)
	{
		this.trim = trim;
	}

	public void setIgnoreEmptyLines(boolean ignoreEmptyLines)
	{
		this.ignoreEmptyLines = ignoreEmptyLines;
	}

	public void doWork() throws BuildException
	{
		String fullPath = getFileFullPath(file);
		if (fullPath == null)
		{
			throw new BuildException("File should be specified");
		}

		if (iterationProperty == null)
		{
			throw new BuildException("Line text property should be specified");
		}

		InputStream in = null;
		BufferedReader reader = null;

		try
		{
			File file = new File(fullPath);
			if (!file.isFile() || !file.exists())
			{
				throw new BuildException("Incorrect file specified: " + fullPath);
			}

			int fileLength = (int)file.length();
			if (fileLength == 0)
			{
				return;
			}

			in = new FileInputStream(file);
			reader = encoding == null ?
					new BufferedReader(new InputStreamReader(in)) :
					new BufferedReader(new InputStreamReader(in, encoding));

			String line;
			long number = -1;

			while ((line = reader.readLine()) != null)
			{
				number++;

				if (startFromLine > number)
				{
					continue;
				}

				if (ignoreEmptyLines && line.trim().isEmpty())
				{
					continue;
				}

				this.setPropertyThreadSafe(iterationProperty, trim ? line.trim() : line);

				if (numberProperty != null)
				{
					this.setPropertyThreadSafe(numberProperty, Long.toString(number));
				}

				for (Task task : tasks)
				{
					task.perform();
				}
    		}
		}
		catch (FileNotFoundException e)
		{
			throw new BuildException("Failed to open file " + fullPath, e);
		}
		catch (IOException e)
		{
			throw new BuildException("Failed read file " + fullPath, e);
		}
		finally
		{
			if (reader != null)
			{
				try
				{
					reader.close();
				}
				catch (Throwable ex) {}
			}

			if (in != null)
			{
				try
				{
					in.close();
				}
				catch (Throwable ex) {}
			}
		}
	}
}
