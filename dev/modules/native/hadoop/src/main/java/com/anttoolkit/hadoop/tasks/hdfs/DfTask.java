package com.anttoolkit.hadoop.tasks.hdfs;

import java.io.*;

import org.apache.hadoop.fs.*;
import org.apache.tools.ant.*;

import com.anttoolkit.hadoop.tasks.hadoop.*;
import com.anttoolkit.hadoop.tasks.hdfs.util.*;

public class DfTask extends GenericHadoopTask
{
	private String uri;
	private boolean humanReadable = false;
	private String sizeProperty;
	private String usedProperty;
	private String freeProperty;
	private String usedPercentProperty;

	public void setUri(String uri)
	{
		this.uri = uri;
	}

	public void setHumanReadable(boolean humanReadable)
	{
		this.humanReadable = humanReadable;
	}

	public void setSizeProperty(String property)
	{
		sizeProperty = property;
	}

	public void setUsedProperty(String property)
	{
		usedProperty = property;
	}

	public void setFreeProperty(String property)
	{
		freeProperty = property;
	}

	public void setUsedPercentProperty(String property)
	{
		usedPercentProperty = property;
	}

	@Override
	protected void doHadoopWork() throws BuildException
	{
		try
		{
			FsStatus status = getRemoteFileSystem().getStatus(new Path(uri));

			if (sizeProperty != null)
			{
				this.setPropertyThreadSafe(sizeProperty, HdfsHelper.formatSize(status.getCapacity(), humanReadable));
			}

			if (usedProperty != null)
			{
				this.setPropertyThreadSafe(usedProperty, HdfsHelper.formatSize(status.getUsed(), humanReadable));
			}

			if (freeProperty != null)
			{
				this.setPropertyThreadSafe(freeProperty, HdfsHelper.formatSize(status.getRemaining(), humanReadable));
			}

			if (usedPercentProperty != null)
			{
				this.setPropertyThreadSafe(freeProperty, Integer.toString((int) ((double) status.getUsed() / (double) status.getCapacity())));
			}
		}
		catch (IOException e)
		{
			throw new BuildException("Failed to get status for: " + uri, e);
		}
	}

	@Override
	protected void hadoopValidate()
	{
		if (uri == null)
		{
			throw new BuildException("URI should be specified");
		}
	}
}
