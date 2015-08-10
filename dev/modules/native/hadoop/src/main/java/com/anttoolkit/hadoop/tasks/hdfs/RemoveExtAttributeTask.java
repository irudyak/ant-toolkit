package com.anttoolkit.hadoop.tasks.hdfs;

import java.io.*;
import java.util.*;

import org.apache.hadoop.fs.*;
import org.apache.tools.ant.*;

import com.anttoolkit.hadoop.tasks.hadoop.*;
import com.anttoolkit.general.common.*;

public class RemoveExtAttributeTask extends GenericHadoopTask
{
	private String path;
	private List<String> attributes = new LinkedList<String>();

	public void setPath(String path)
	{
		this.path = path;
	}

	public void addConfiguredAttribute(ValueHolder attribute)
	{
		if (!attributes.contains(attribute.getValue()))
		{
			attributes.add(attribute.getValue());
		}
	}

	@Override
	protected void doHadoopWork() throws BuildException
	{
		Path hdfsPath = new Path(path);

		for (String attribute : attributes)
		{
			try
			{
				getRemoteFileSystem().removeXAttr(hdfsPath, attribute);
			}
			catch (IOException e)
			{
				throw new BuildException("Failed to remove extended attribute \"" + attribute + "\" from path \"" + path + "\"", e);
			}
		}
	}

	@Override
	protected void hadoopValidate()
	{
		if (path == null || path.trim().isEmpty())
		{
			throw new BuildException("Path should be specified");
		}

		if (attributes.isEmpty())
		{
			throw new BuildException("Extended attributes should be specified");
		}
	}
}
