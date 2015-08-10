package com.anttoolkit.hbase.tasks.janitor;

import org.apache.tools.ant.*;

import com.anttoolkit.hbase.tasks.*;

public class CheckCatalogJanitorEnabledTask extends GenericHBaseTask
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
			setPropertyThreadSafe(property, Boolean.toString(getHBaseAdmin().isCatalogJanitorEnabled()));
		}
		catch (Exception e)
		{
			throw new BuildException("Failed to check if catalog janitor enabled", e);
		}
	}

	@Override
	protected void hadoopValidate()
	{
		if (property == null || property.trim().isEmpty())
		{
			throw new BuildException("Property should be specified");
		}
	}
}
