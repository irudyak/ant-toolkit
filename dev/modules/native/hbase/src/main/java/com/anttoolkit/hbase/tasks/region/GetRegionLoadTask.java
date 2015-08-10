package com.anttoolkit.hbase.tasks.region;

import org.apache.hadoop.hbase.*;
import org.apache.tools.ant.*;

import com.anttoolkit.hbase.tasks.*;
import com.anttoolkit.general.refs.*;

public class GetRegionLoadTask extends GenericHBaseTask
{
	private String reference;
	private String server;
	private String region;

	public void setReference(String ref)
	{
		reference = ref;
	}

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
		RegionLoad load = server != null ? getRegionLoad(server, region) : getRegionLoad(region);

		if (load == null)
		{
			throw server != null ?
					new BuildException("Failed to locate region " + region + " on HBase server " + server) :
					new BuildException("Failed to locate region " + region);
		}

		this.setReference(reference, load);
	}

	@Override
	protected void hadoopValidate()
	{
		if (reference == null || reference.trim().isEmpty())
		{
			throw new BuildException("Reference name should be specified");
		}

		if (region == null || region.trim().isEmpty())
		{
			throw new BuildException("Region name should be specified");
		}
	}
}
