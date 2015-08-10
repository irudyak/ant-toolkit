package com.anttoolkit.hbase.tasks.region;

import org.apache.tools.ant.*;

import com.anttoolkit.hbase.tasks.*;

public class SplitRegionTask extends GenericHBaseTask
{
	private String region;
	private String splitPoint;

	public void setRegion(String region)
	{
		this.region = region;
	}

	public void setSplitPoint(String splitPoint)
	{
		this.splitPoint = splitPoint;
	}

	@Override
	protected void doHadoopWork() throws BuildException
	{
		try
		{
			if (!checkRegionExists(region))
			{
				throw new BuildException("Region " + region + " doesn't exist");
			}

			if (splitPoint != null)
			{
				getHBaseAdmin().split(region, splitPoint);
			}
			else
			{
				getHBaseAdmin().split(region);
			}
		}
		catch (Exception e)
		{
			throw new BuildException("Failed to split region " + region);
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
