package com.anttoolkit.hbase.tasks.namespace;

import java.io.*;
import java.util.*;

import org.apache.hadoop.hbase.*;
import org.apache.tools.ant.*;

import com.anttoolkit.hbase.tasks.*;

public class CreateNamespaceTask extends GenericHBaseTask
{
	private String name;
	private List<KeyValuePair> metadata = new LinkedList<KeyValuePair>();

	public void setName(String name)
	{
		this.name = name;
	}

	public void addConfiguredMetadata(KeyValuePair value)
	{
		if (value == null)
		{
			throw new IllegalArgumentException("Metadata can't be null");
		}

		value.validate();
		metadata.add(value);
	}

	@Override
	protected void doHadoopWork() throws BuildException
	{
		NamespaceDescriptor.Builder builder = NamespaceDescriptor.create(name);
		for (KeyValuePair pair : metadata)
		{
			builder.addConfiguration(pair.getKey(), pair.getValue());
		}

		try
		{
			getHBaseAdmin().createNamespace(builder.build());
		}
		catch (IOException e)
		{
			throw new BuildException("Failed to create namespace " + name, e);
		}
	}

	protected void hadoopValidate()
	{
		if (name == null || name.trim().isEmpty())
		{
			throw new BuildException("Namespace name should be specified");
		}
	}
}
