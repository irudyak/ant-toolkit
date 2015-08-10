package com.anttoolkit.hadoop.tasks.hdfs;

import java.io.*;
import java.util.*;

import org.apache.hadoop.fs.*;
import org.apache.tools.ant.*;

import com.anttoolkit.hadoop.tasks.hadoop.*;
import com.anttoolkit.general.common.*;

public class GetExtAttributeTask  extends GenericHadoopTask
{
	private String path;
	private Map<String, String> attributes = new HashMap<String, String>();

	public void setPath(String path)
	{
		this.path = path;
	}

	public void addConfiguredAttribute(NamePropertyHolder attribute)
	{
		attributes.put(attribute.getName(), attribute.getProperty());
	}

	@Override
	protected void doHadoopWork() throws BuildException
	{
		Map<String, byte[]> attrs = null;

		try
		{
			attrs = getRemoteFileSystem().getXAttrs(new Path(path), new LinkedList(attributes.keySet()));
		}
		catch (IOException e)
		{
			throw new BuildException("Failed to get extended attributes for path \"" + path + "\"", e);
		}

		for (String name : attrs.keySet())
		{
			String property = attributes.get(name);
			byte[] value = attrs.get(name);

			if (value == null)
			{
				continue;
			}

			if (value.length == 0)
			{
				this.setPropertyThreadSafe(property, "");
				continue;
			}

			try
			{
				this.setPropertyThreadSafe(property, XAttrCodec.encodeValue(value, XAttrCodec.TEXT));
			}
			catch (IOException e)
			{
				throw new BuildException("Failed to decode value of \"" + name + "\" extended attribute", e);
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
