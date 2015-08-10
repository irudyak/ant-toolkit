package com.anttoolkit.hbase.tasks.region;

import org.apache.tools.ant.*;

import com.anttoolkit.hbase.tasks.*;

public class MajorCompactRegionTask extends GenericHBaseTask
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
			if (checkRegionExists(region))
			{
				throw new BuildException("Region " + region + " doesn't exist");
			}

			getHBaseAdmin().majorCompact(region);
		}
		catch (Exception e)
		{
			throw new BuildException("Failed to major compact region " + region);
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
