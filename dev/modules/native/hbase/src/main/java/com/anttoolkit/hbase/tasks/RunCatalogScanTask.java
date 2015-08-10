package com.anttoolkit.hbase.tasks;

import org.apache.tools.ant.*;

public class RunCatalogScanTask extends GenericHBaseTask
{
	private String property;

	public void setProperty(String property)
	{
		this.property = property;
	}

	@Override
	protected void doHadoopWork() throws BuildException
	{
		try
		{
			int entries = getHBaseAdmin().runCatalogScan();
			if (property != null)
			{
				setPropertyThreadSafe(property, Integer.toString(entries));
			}
		}
		catch (Exception e)
		{
			throw new BuildException("Failed to run catalog scan", e);
		}
	}
}
