package com.anttoolkit.hbase.tasks.region;

import org.apache.hadoop.hbase.util.*;
import org.apache.tools.ant.*;

import com.anttoolkit.hbase.tasks.*;

public class UnassignRegionTask extends GenericHBaseTask
{
	private String region;
	private boolean force = false;

	public void setRegion(String region)
	{
		this.region = region;
	}

	public void setForce(boolean force)
	{
		this.force = force;
	}

	@Override
	protected void doHadoopWork() throws BuildException
	{
		try
		{
			getHBaseAdmin().unassign(Bytes.toBytes(region), force);
		}
		catch (Exception e)
		{
			throw new BuildException("Failed to unassign region " + region, e);
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
