package com.anttoolkit.hbase.tasks.server;

import java.io.*;

import org.apache.hadoop.hbase.util.*;
import org.apache.tools.ant.*;

import com.anttoolkit.hbase.tasks.*;

public class StopServerTask extends GenericHBaseTask
{
	private String host;
	private int port = -1;

	public void setHost(String host)
	{
		this.host = host;
	}

	public void setPort(int port)
		{
			this.port = port;
		}

	@Override
	protected void doHadoopWork() throws BuildException
	{
		try
		{
			getHBaseAdmin().stopRegionServer(Addressing.createHostAndPortStr(host, port));
		}
		catch (IOException e)
		{
			throw new BuildException("Failed to stop server " + host + ":" + port);
		}
	}

	@Override
	protected void hadoopValidate()
	{
		if (host == null || host.trim().isEmpty())
		{
			throw new BuildException("Server host should be specified");
		}

		if (port == -1)
		{
			throw new BuildException("Server port should be specified");
		}
	}
}
