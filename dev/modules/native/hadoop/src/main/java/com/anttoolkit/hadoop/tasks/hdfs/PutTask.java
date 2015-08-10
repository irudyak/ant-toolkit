package com.anttoolkit.hadoop.tasks.hdfs;

import java.io.*;

import org.apache.hadoop.fs.*;
import org.apache.hadoop.io.*;
import org.apache.tools.ant.*;

import com.anttoolkit.hadoop.tasks.hadoop.*;

public class PutTask extends GenericHadoopTask
{
	private String src;
	private String dest;
	private boolean overwrite = false;

	public void setSrc(String src)
	{
		this.src = this.getFileFullPath(src);
	}

	public void setDest(String dest)
	{
		this.dest = dest;
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

			FileStatus destStatus = getFileStatus(dest);
			if (destStatus != null && destStatus.isDirectory())
			{
				String _src = src.replace("\\", "/");
				_src = _src.lastIndexOf("/") == -1 ? _src : _src.substring(_src.lastIndexOf("/") + 1);

				destFile = !dest.endsWith("/") ? dest + "/" : dest;
				destFile += _src;
			}

			in = getLocalFileSystem().open(new Path(src));
			out = getRemoteFileSystem().create(new Path(destFile), overwrite);
			IOUtils.copyBytes(in, out, getRemoteFileSystem().getConf(), true);
		}
		catch (IOException e)
		{
			throw new BuildException("Failed to put source file '" + src + "' to destination '" + dest + "'", e);
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

		File file = new File(src);
		if (!file.exists() || file.isDirectory())
		{
			throw new IllegalArgumentException("Incorrect source file specified: " + src);
		}

		if (dest == null || dest.trim().isEmpty())
		{
			throw new BuildException("Destination file should be specified");
		}

		FileStatus status = getFileStatus(dest);
		if (status == null)
		{
			return;
		}

		if (!overwrite && status.isFile())
		{
			throw new BuildException("Cant copy file '" + src + "' to destination '" + dest + "', cause file with the same name already exists");
		}
	}
}
