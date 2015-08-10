package com.anttoolkit.hadoop.tasks.hdfs;

import java.io.*;

import org.apache.hadoop.fs.*;
import org.apache.hadoop.io.*;
import org.apache.tools.ant.*;

import com.anttoolkit.hadoop.tasks.hadoop.*;

public class GetTask
		extends GenericHadoopTask
{
	private String src;
	private String dest;
	private boolean overwrite = false;

	public void setSrc(String src)
	{
		this.src = src;
	}

	public void setDest(String dest)
	{
		this.dest = this.getFileFullPath(dest);
	}

	public void setOverwrite(boolean overwrite)
	{
		this.overwrite = overwrite;
	}

	@Override
	public void doHadoopWork() throws BuildException
	{
		OutputStream out = null;
		InputStream in = null;

		try
		{
			String destFile = dest;

			File file = new File(dest);
			if (file.exists() && file.isDirectory())
			{
				String _src = src.lastIndexOf("/") == -1 ? src : src.substring(src.lastIndexOf("/") + 1);

				destFile = !dest.endsWith("/") ? dest + "/" : dest;
				destFile += _src;
			}

			in = getRemoteFileSystem().open(new Path(src));
			out = new FileOutputStream(destFile, false);
			IOUtils.copyBytes(in, out, getRemoteFileSystem().getConf(), true);
		}
		catch (IOException e)
		{
			throw new BuildException("Failed to get source file '" + src + "' to destination '" + dest + "'", e);
		}
		finally
		{
			IOUtils.closeStream(in);
			IOUtils.closeStream(out);
		}
	}

	@Override
	protected void hadoopValidate()
	{
		if (src == null || src.trim().isEmpty())
		{
			throw new BuildException("Source file should be specified");
		}

		FileStatus status = getFileStatus(src);
		if (status == null)
		{
			throw new BuildException("Specified source file doesn't exist: " + src);
		}

		if (status.isDirectory())
		{
			throw new IllegalArgumentException("Cant get file '" + src + "', cause it's directory");
		}

		if (dest == null || dest.trim().isEmpty())
		{
			throw new BuildException("Destination file should be specified");
		}

		File file = new File(dest);
		if (!file.exists())
		{
			return;
		}

		if (!overwrite && file.isFile())
		{
			throw new IllegalArgumentException("Incorrect destination file specified '" + dest + "' cause file with the same name already exists");
		}
	}

}
