package com.anttoolkit.hbase.tasks.namespace;

import java.io.*;

import org.apache.hadoop.hbase.*;
import org.apache.tools.ant.*;

import com.anttoolkit.hbase.tasks.*;

public class CheckNamespaceExistsTask extends GenericHBaseTask
{
	private String namespace;
	private String property;

	public void setNamespace(String namespace)
	{
		this.namespace = namespace;
	}

	public void setProperty(String property)
	{
		this.property = property;
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
			setPropertyThreadSafe(property, Boolean.FALSE.toString());
			return;
		}

		for (NamespaceDescriptor descr : descriptors)
		{
			if (descr.getName().equals(namespace))
			{
				setPropertyThreadSafe(property, Boolean.TRUE.toString());
				return;
			}
		}

		setPropertyThreadSafe(property, Boolean.FALSE.toString());
	}

	@Override
	protected void hadoopValidate()
	{
		if (property == null || property.trim().isEmpty())
		{
			throw new BuildException("Property name should be specified");
		}

		if (namespace == null || namespace.trim().isEmpty())
		{
			throw new BuildException("Namespace should be specified");
		}
	}
}
