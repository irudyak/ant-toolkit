package com.anttoolkit.hadoop.tasks.mapred.util;

import org.apache.tools.ant.*;
import org.apache.hadoop.fs.*;
import org.apache.hadoop.mapreduce.*;
import org.apache.hadoop.mapreduce.lib.input.*;

import com.anttoolkit.general.common.*;

public class InputPath
{
	private Class<? extends InputFormat> formatClass;
	private Class<? extends Mapper> mapperClass;
	private String path;

	public void setFormatClass(String clazz)
	{
		try
		{
			Object obj = ReflectionHelper.forName(clazz);
			this.formatClass = (Class<? extends InputFormat>)obj;
		}
		catch (ClassNotFoundException e)
		{
			throw new BuildException("Failed to find input format class '" + clazz + "'", e);
		}
	}

	public void setMapperClass(String clazz)
	{
		try
		{
			Object obj = ReflectionHelper.forName(clazz);
			this.mapperClass = (Class<? extends Mapper>)obj;
		}
		catch (ClassNotFoundException e)
		{
			throw new BuildException("Failed to find mapper class '" + clazz + "'", e);
		}
	}

	public void setPath(String path)
	{
		this.path = path;
	}

	public void addText(String path)
	{
		this.setPath(path.trim());
	}

	public void addInputPath(Job job)
	{
		if (mapperClass != null)
		{
			MultipleInputs.addInputPath(job, new Path(path), formatClass, mapperClass);
		}
		else
		{
			MultipleInputs.addInputPath(job, new Path(path), formatClass);
		}
	}

	private void validate()
	{
		if (path == null || path.trim().isEmpty())
		{
			throw new BuildException("Path should be specified for MapReduce job input");
		}

		if (formatClass == null)
		{
			throw new BuildException("Format class should be specified for MapReduce multiple job input");
		}
	}
}
