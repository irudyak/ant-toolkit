package com.anttoolkit.hbase.tasks.namespace;

import java.io.*;

import org.apache.tools.ant.*;

import com.anttoolkit.hbase.tasks.*;

public class DeleteNamespaceTask extends GenericHBaseTask
{
	private String namespace;

	public void setNamespace(String namespace)
	{
		this.namespace = namespace;
	}

	@Override
	protected void doHadoopWork() throws BuildException
	{
		try
		{
			getHBaseAdmin().deleteNamespace(namespace);
		}
		catch (IOException e)
		{
			throw new BuildException("Failed to delete " + namespace + " namespace", e);
		}
	}

	@Override
	protected void hadoopValidate()
	{
		if (namespace == null || namespace.trim().isEmpty())
		{
			throw new BuildException("Namespace should be specified");
		}
	}
}
