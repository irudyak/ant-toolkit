package com.anttoolkit.hbase.tasks.region;

import org.apache.tools.ant.*;

import com.anttoolkit.hbase.tasks.*;


public class FlushRegionTask extends GenericHBaseTask
{
	private String region;

	public void setRegion(String region)
	{
		this.region = region;
	}

	@Override
	protected void doHadoopWork() throws BuildException
	{
		if (!checkRegionExists(region))
		{
			throw new BuildException("Failed to find " + region + " region");
		}

		try
		{
			getHBaseAdmin().flush(region);
		}
		catch (Exception e)
		{
			throw new BuildException("Failed to flash region " + region, e);
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
