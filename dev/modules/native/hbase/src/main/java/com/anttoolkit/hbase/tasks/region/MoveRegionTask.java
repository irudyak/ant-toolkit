package com.anttoolkit.hbase.tasks.region;

import org.apache.hadoop.hbase.util.*;
import org.apache.tools.ant.*;

import com.anttoolkit.hbase.tasks.*;

public class MoveRegionTask extends GenericHBaseTask
{
	private String server;
	private String region;

	public void setServer(String name)
	{
		server = name;
	}

	public void setRegion(String region)
	{
		this.region = region;
	}

	@Override
	protected void doHadoopWork() throws BuildException
	{
		try
		{
			getHBaseAdmin().move(Bytes.toBytes(region), Bytes.toBytes(server));
		}
		catch (Exception e)
		{
			throw new BuildException("Failed to move region " + region + " to server " + server);
		}
	}

	protected void hadoopValidate()
	{
		if (region == null || region.trim().isEmpty())
		{
			throw new BuildException("Region name should be specified");
		}

		if (server == null || server.trim().isEmpty())
		{
			throw new BuildException("Server name should be specified");
		}
	}
}
