package com.anttoolkit.hbase.tasks.region;

import org.apache.hadoop.hbase.protobuf.generated.AdminProtos.GetRegionInfoResponse.CompactionState;
import org.apache.tools.ant.*;

import com.anttoolkit.hbase.tasks.*;

public class GetRegionCompactionStateTask extends GenericHBaseTask
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
		try
		{
			if (!checkRegionExists(region))
			{
				throw new BuildException("Region " + region + " doesn't exist");
			}

			CompactionState state = getHBaseAdmin().getCompactionState(region);

			setPropertyThreadSafe(property, state.toString());
		}
		catch (Exception e)
		{
			throw new BuildException("Failed to get " + region + " region compaction state", e);
		}
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
			throw new BuildException("Property should be specified");
		}
	}
}
