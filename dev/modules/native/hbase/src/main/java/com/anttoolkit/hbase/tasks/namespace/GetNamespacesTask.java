package com.anttoolkit.hbase.tasks.namespace;

import java.io.*;

import org.apache.hadoop.hbase.*;
import org.apache.tools.ant.*;

import com.anttoolkit.hbase.tasks.*;
import com.anttoolkit.general.tasks.collections.array.util.*;

public class GetNamespacesTask extends GenericHBaseTask
{
	private String array;

	public void setArray(String array)
	{
		this.array = array;
	}

	@Override
	protected void doHadoopWork() throws BuildException
	{
		NamespaceDescriptor[] descriptors;

		try
		{
			descriptors = getHBaseAdmin().listNamespaceDescriptors();
		}
		catch (IOException e)
		{
			throw new BuildException("Failed to get list of namespace descriptors");
		}

		if (descriptors == null)
		{
			return;
		}

		for (NamespaceDescriptor descr : descriptors)
		{
			ArrayManager.add(array, descr.getName());
		}
	}

	@Override
	protected void hadoopValidate()
	{
		if (array == null || array.trim().isEmpty())
		{
			throw new BuildException("Array name should be specified");
		}

		if (!ArrayManager.exists(array))
		{
			throw new BuildException("Array " + array + " doesn't exist");
		}
	}
}
