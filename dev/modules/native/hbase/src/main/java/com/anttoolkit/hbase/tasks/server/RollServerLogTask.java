package com.anttoolkit.hbase.tasks.server;

import java.io.*;

import org.apache.tools.ant.*;

import com.anttoolkit.hbase.tasks.*;

public class RollServerLogTask extends GenericHBaseTask
{
	private String server;

	public void setServer(String server)
	{
		this.server = server;
	}

	@Override
	protected void doHadoopWork() throws BuildException
	{
		try
		{
			getHBaseAdmin().rollHLogWriter(server);
		}
		catch (IOException e)
		{
			throw new BuildException("Failed to roll logs on " + server + " server", e);
		}
	}

	@Override
	protected void hadoopValidate()
	{
		if (server == null || server.trim().isEmpty())
		{
			throw new BuildException("Server name should be specified");
		}
	}
}
