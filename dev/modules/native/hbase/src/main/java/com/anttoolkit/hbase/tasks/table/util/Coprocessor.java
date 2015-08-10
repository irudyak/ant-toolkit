package com.anttoolkit.hbase.tasks.table.util;

import java.io.*;
import java.util.*;

import com.anttoolkit.hbase.tasks.KeyValuePair;
import org.apache.tools.ant.*;

import org.apache.hadoop.fs.*;
import org.apache.hadoop.hbase.*;
import org.apache.hadoop.hbase.util.*;

public class Coprocessor
{
	private String className;
	private String jarFilePath;
	private int priority = 0;
	private List<KeyValuePair> params = new LinkedList<KeyValuePair>();

	public void setClassName(String className)
	{
		this.className = className;
	}

	public String getClassName()
	{
		return className;
	}

	public void setJarFilePath(String path)
	{
		this.jarFilePath = path;
	}

	public void setPriority(int priority)
	{
		this.priority = priority;
	}

	public KeyValuePair createParam()
	{
		KeyValuePair pair = new KeyValuePair();
		params.add(pair);
		return pair;
	}

	public void addCorprocessor(HTableDescriptor table)
	{
		validate();

		try
		{
			if (jarFilePath == null || jarFilePath.trim().isEmpty())
			{
				table.addCoprocessor(className);
			}
			else
			{
				table.addCoprocessor(className, new Path(jarFilePath), priority, getParams());
			}
		}
		catch (IOException e)
		{
			throw new BuildException("Failed to add Coprocessor to HBase table " + Bytes.toString(table.getName()), e);
		}
	}

	public void validate()
	{
		if (className == null || className.trim().isEmpty())
		{
			throw new BuildException("Coprocessor class name should be specified");
		}
	}

	private Map<String, String> getParams()
	{
		if (params.isEmpty())
		{
			return new HashMap<String, String>();
		}

		Map<String, String> _params = new HashMap<String, String>();

		for (KeyValuePair pair : params)
		{
			_params.put(pair.getKey(), pair.getValue());
		}

		return _params;
	}
}
