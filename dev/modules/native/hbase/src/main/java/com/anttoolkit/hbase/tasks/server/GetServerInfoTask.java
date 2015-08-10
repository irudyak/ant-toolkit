package com.anttoolkit.hbase.tasks.server;

import org.apache.tools.ant.*;

import com.anttoolkit.general.refs.*;
import com.anttoolkit.hbase.tasks.*;

public class GetServerInfoTask extends GenericHBaseTask
{
	private String reference;
	private String server;

	public void setReference(String ref)
	{
		reference = ref;
	}

	public void setServer(String name)
	{
		server = name;
	}

	@Override
	protected void doHadoopWork() throws BuildException
	{
		this.setReference(reference, getServer(server));
	}

	@Override
	protected void hadoopValidate()
	{
		if (reference == null || reference.trim().isEmpty())
		{
			throw new BuildException("Reference name should be specified");
		}

		if (server == null || server.trim().isEmpty())
		{
			throw new BuildException("Server name should be specified");
		}
	}
}
