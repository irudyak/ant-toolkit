package com.anttoolkit.hbase.tasks;

import org.apache.hadoop.hbase.client.*;
import org.apache.tools.ant.*;

public class CheckHBaseAvailableTask extends GenericHBaseTask
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
			HBaseAdmin.checkHBaseAvailable(getConfiguration());
			setPropertyThreadSafe(property, Boolean.TRUE.toString());
		}
		catch (Throwable e)
		{
			setPropertyThreadSafe(property, Boolean.FALSE.toString());
		}
	}

	@Override
	protected void hadoopValidate()
	{
		if (property == null)
		{
			throw new BuildException("Property should be specified");
		}
	}
}
