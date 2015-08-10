package com.anttoolkit.hbase.tasks.region;

import java.io.*;

import org.apache.hadoop.hbase.util.*;
import org.apache.tools.ant.*;

import com.anttoolkit.hbase.tasks.*;

public class MergeRegionsTask extends GenericHBaseTask
{
	private String region1;
	private String region2;
	private boolean forcible = false;

	public void setRegion1(String region)
	{
		region1 = region;
	}

	public void setRegion2(String region)
	{
		region2 = region;
	}

	public void setForcible(boolean forcible)
	{
		this.forcible = forcible;
	}

	@Override
	protected void doHadoopWork() throws BuildException
	{
		try
		{
			getHBaseAdmin().mergeRegions(Bytes.toBytes(region1), Bytes.toBytes(region2), forcible);
		}
		catch (IOException e)
		{
			throw new BuildException("Failed to merge region " + region1 + " and region " + region2, e);
		}
	}

	protected void hadoopValidate()
	{
		if (region1 == null || region1.trim().isEmpty())
		{
			throw new BuildException("Region1 name should be specified");
		}

		if (region2 == null || region2.trim().isEmpty())
		{
			throw new BuildException("Region2 name should be specified");
		}
	}
}
