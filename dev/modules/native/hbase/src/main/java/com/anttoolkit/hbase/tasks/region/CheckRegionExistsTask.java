package com.anttoolkit.hbase.tasks.region;

import org.apache.tools.ant.*;

import com.anttoolkit.hbase.tasks.*;

public class CheckRegionExistsTask extends GenericHBaseTask
{
	private String region;
	private String property;

	public void setRegion(String region)
	{
		this.region = region;
	}

	public void setProperty(String property)
	{
		this.property = property;
	}

	@Override
	protected void doHadoopWork() throws BuildException
	{
		Boolean exists = checkRegionExists(region);
		setPropertyThreadSafe(property, exists.toString());
	}

	@Override
	protected void hadoopValidate()
	{
		if (property == null || property.trim().isEmpty())
		{
			throw new BuildException("Property name should be specified");
		}

		if (region == null || region.trim().isEmpty())
		{
			throw new BuildException("Region name should be specified");
		}
	}
}