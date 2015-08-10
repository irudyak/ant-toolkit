package com.anttoolkit.hbase.tasks;

import org.apache.tools.ant.*;

public class CheckMasterRunningTask extends GenericHBaseTask
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
			setPropertyThreadSafe(property, Boolean.toString(getHBaseAdmin().isMasterRunning()));
		}
		catch (Exception e)
		{
			throw new BuildException("Failed to check if master is running", e);
		}
	}

	protected void hadoopValidate()
	{
		if (property == null || property.trim().isEmpty())
		{
			throw new BuildException("Property name should be specified");
		}
	}
}
