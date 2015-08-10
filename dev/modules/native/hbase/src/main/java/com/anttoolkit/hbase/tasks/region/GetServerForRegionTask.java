package com.anttoolkit.hbase.tasks.region;

import org.apache.tools.ant.*;

import com.anttoolkit.hbase.tasks.*;

public class GetServerForRegionTask extends GenericHBaseTask
{
	private String property;
	private String region;

	public void setProperty(String property)
	{
		this.property = property;
	}

	public void setRegion(String region)
	{
		this.region = region;
	}

	@Override
	protected void doHadoopWork() throws BuildException
	{
		String server = getServerForRegion(region);
		if (server == null)
		{
			throw new BuildException("Unable to find HBase server for region " + region);
		}

		setPropertyThreadSafe(property, server);
	}

	@Override
	protected void hadoopValidate()
	{
		if (region == null || region.trim().isEmpty())
		{
			throw new BuildException("Region name should be specified");
		}

		if (property == null || property.trim().isEmpty())
		{
			throw new BuildException("Property name should be specified");
		}
	}
}
