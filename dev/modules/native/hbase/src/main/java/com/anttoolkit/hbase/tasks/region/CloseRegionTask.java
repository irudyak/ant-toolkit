package com.anttoolkit.hbase.tasks.region;

import java.io.*;

import org.apache.tools.ant.*;

import com.anttoolkit.hbase.tasks.*;

public class CloseRegionTask extends GenericHBaseTask
{
	private String region;

	public void setRegion(String region)
	{
		this.region = region;
	}

	@Override
	protected void doHadoopWork() throws BuildException
	{
		String serverName = getServerForRegion(region);
		if (serverName == null)
		{
			throw new BuildException("Unable to find HBase server for region " + region);
		}

		try
		{
			getHBaseAdmin().closeRegion(region, serverName);
		}
		catch (IOException e)
		{
			throw new BuildException("Failed to close region " + region, e);
		}
	}

	@Override
	protected void hadoopValidate()
	{
		if (region == null || region.trim().isEmpty())
		{
			throw new BuildException("Region name should be specified");
		}
	}
}
