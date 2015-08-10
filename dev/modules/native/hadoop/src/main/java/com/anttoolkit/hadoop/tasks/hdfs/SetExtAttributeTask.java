package com.anttoolkit.hadoop.tasks.hdfs;

import java.io.*;
import java.util.*;

import org.apache.hadoop.fs.*;
import org.apache.tools.ant.*;

import com.anttoolkit.hadoop.tasks.hadoop.*;
import com.anttoolkit.general.common.*;

public class SetExtAttributeTask extends GenericHadoopTask
{
	private String path;
	private Map<String, byte[]> attributes = new HashMap<String, byte[]>();

	public void setPath(String path)
	{
		this.path = path;
	}

	public void addConfiguredAttribute(NameValueHolder attribute)
	{
		try
		{
			attributes.put(attribute.getName(), XAttrCodec.decodeValue(attribute.getValue()));
		}
		catch (IOException e)
		{
			throw new BuildException("Failed to decode extended attribute \"" + attribute.getName() + "\" value \"" + attribute.getValue() + "\"", e);
		}
	}

	@Override
	protected void doHadoopWork() throws BuildException
	{
		Path hdfsPath = new Path(path);

		for (Map.Entry<String, byte[]> entry : attributes.entrySet())
		{
			try
			{
				getRemoteFileSystem().setXAttr(hdfsPath, entry.getKey(), entry.getValue());
			}
			catch (IOException e)
			{
				throw new BuildException("Failed to set extended attribute \"" + entry.getKey() + "\" on path \"" + path + "\"", e);
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
