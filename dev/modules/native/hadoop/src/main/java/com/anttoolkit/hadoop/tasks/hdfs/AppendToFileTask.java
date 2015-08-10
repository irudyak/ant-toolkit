package com.anttoolkit.hadoop.tasks.hdfs;

import java.io.*;
import java.util.*;

import org.apache.hadoop.fs.*;
import org.apache.hadoop.io.*;
import org.apache.tools.ant.*;

import com.anttoolkit.general.common.*;
import com.anttoolkit.hadoop.tasks.hadoop.*;

public class AppendToFileTask extends GenericHadoopTask
{
	private List<String> sources = new LinkedList<String>();
	private String dest;

	public void setSrc(String src)
	{
		if (src == null)
		{
			return;
		}

		String localFile = this.getFileFullPath(src);

		if (!sources.contains(localFile))
		{
			sources.add(localFile);
		}
	}

	public void setDest(String dest)
	{
		this.dest = dest;
	}

	public void addConfiguredSrc(ValueHolder src)
	{
		String localFile = this.getFileFullPath(src.getValue());

		if (!sources.contains(localFile))
		{
			sources.add(localFile);
		}
	}

	@Override
	protected void doHadoopWork() throws BuildException
	{
		if (getFileStatus(dest) == null)
		{
			try
			{
				getRemoteFileSystem().create(new Path(dest), false).close();
			}
			catch (IOException e)
			{
				throw new BuildException("Failed to create file on remote filesystem: " + dest, e);
			}
		}

		InputStream in = null;
		FSDataOutputStream out = null;

		try
		{
			out = getRemoteFileSystem().append(new Path(dest));
		}
		catch (IOException e)
		{
			throw new BuildException("Failed to open remote file in append mode: " + dest, e);
		}

		try
		{
			for (String src : sources)
			{
				try
				{
					in = new FileInputStream(src);
				}
				catch (FileNotFoundException e)
				{
					throw new BuildException("Failed to find source file: " + src, e);
				}

				try
				{
					IOUtils.copyBytes(in, out, DEFAULT_IO_LENGTH);
				}
				catch (IOException e)
				{
					throw new BuildException("Failed to append local file \"" + src + "\" to " +
							"remote file \"" + dest + "\"", e);
				}

				IOUtils.closeStream(in);
				in = null;
			}
		}
		finally
		{
			if (out != null)
			{
				IOUtils.closeStream(out);
			}

			if (in != null)
			{
				IOUtils.closeStream(in);
			}
		}
	}

	protected void hadoopValidate()
	{
		if (sources.isEmpty())
		{
			throw new BuildException("No sources specified for AppendToFile operation");
		}

		if (dest == null)
		{
			throw new BuildException("No destination specified for AppendToFile operation");
		}

		for (String src : sources)
		{
			File file = new File(src);

			if (!file.exists())
			{
				throw new BuildException("Specified source file doesn't exist: " + src);
			}

			if (file.isDirectory())
			{
				throw new BuildException("Specified source file is actually a directory: " + src);
			}
		}
	}
}
