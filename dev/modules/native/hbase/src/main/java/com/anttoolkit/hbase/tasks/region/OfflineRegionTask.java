package com.anttoolkit.hbase.tasks.region;

import java.io.*;

import org.apache.hadoop.hbase.util.*;
import org.apache.tools.ant.*;

import com.anttoolkit.hbase.tasks.*;

public class OfflineRegionTask extends GenericHBaseTask
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
			getHBaseAdmin().offline(Bytes.toBytes(region));
		}
		catch (IOException e)
		{
			throw new BuildException("Failed to put " + region + " offline", e);
		}
	}

	protected void hadoopValidate()
	{
		if (region == null || region.trim().isEmpty())
		{
			throw new BuildException("Region name should be specified");
		}
	}
}
