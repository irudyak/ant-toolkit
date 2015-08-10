package com.anttoolkit.hadoop.tasks.hdfs;

import java.io.*;

import org.apache.hadoop.fs.*;
import org.apache.tools.ant.*;

import com.anttoolkit.hadoop.tasks.hadoop.*;

public class ExpungeTask extends GenericHadoopTask
{
	@Override
	protected void doHadoopWork() throws BuildException
	{
		FileSystem[] childFileSystems = getRemoteFileSystem().getChildFileSystems();

		try
		{
			if (null == childFileSystems)
			{
				Trash trash = new Trash(this.getConfiguration());
				trash.expunge();
				trash.checkpoint();

				return;
			}

			for (FileSystem fs : childFileSystems)
			{
				Trash trash = new Trash(fs, this.getConfiguration());
				trash.expunge();
				trash.checkpoint();
			}
		}
		catch (IOException e)
		{
			throw new BuildException("Failed to empty the trash", e);
		}
	}
}
