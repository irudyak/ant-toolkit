package com.anttoolkit.hbase.tasks.region;

import org.apache.tools.ant.*;

import com.anttoolkit.hbase.tasks.*;

public class CompactRegionTask extends GenericHBaseTask
{
	private String region;

	public void setRegion(String region)
	{
		this.region = region;
	}

	@Override
	protected void doHadoopWork() throws BuildException
	{
		try
		{
			getHBaseAdmin().compact(region);
		}
		catch (Exception e)
		{
			throw new BuildException("Failed to compact region " + region, e);
		}
	}

	@Override
	protected void hadoopValidate()
	{
		if (region == null || region.trim().isEmpty())
		{
			throw new BuildException("Region name should be specified");
		}

		if (checkRegionExists(region))
		{
			throw new BuildException("Region doesn't exist");
		}
	}
}
