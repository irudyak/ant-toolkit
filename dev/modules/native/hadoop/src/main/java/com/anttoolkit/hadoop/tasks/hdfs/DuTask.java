package com.anttoolkit.hadoop.tasks.hdfs;

import java.io.*;

import org.apache.hadoop.fs.*;
import org.apache.tools.ant.*;

import com.anttoolkit.hadoop.tasks.hadoop.*;
import com.anttoolkit.hadoop.tasks.hdfs.util.*;

public class DuTask extends GenericHadoopTask
{
	private String uri;
	private boolean humanReadable = false;
	private String property;

	public void setUri(String uri)
	{
		this.uri = uri;
	}

	public void setHumanReadable(boolean humanReadable)
	{
		this.humanReadable = humanReadable;
	}

	public void setProperty(String property)
	{
		this.property = property;
	}

	@Override
	protected void doHadoopWork() throws BuildException
	{
		try
		{
			ContentSummary summary = getRemoteFileSystem().getContentSummary(new Path(uri));
			this.setPropertyThreadSafe(property, HdfsHelper.formatSize(summary.getSpaceConsumed(), humanReadable));
		}
		catch (IOException e)
		{
			throw new BuildException("Failed to get content summary info for: " + uri, e);
		}
	}

	@Override
	protected void hadoopValidate()
	{
		if (uri == null)
		{
			throw new BuildException("URI should be specified");
		}

		if (property == null)
		{
			throw new BuildException("Ant property to store URI size should be specified");
		}
	}
}
