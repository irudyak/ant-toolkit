package com.anttoolkit.hbase.tasks.janitor;

import org.apache.tools.ant.*;

import com.anttoolkit.hbase.tasks.*;

public class EnableCatalogJanitorTask extends GenericHBaseTask
{
	@Override
	protected void doHadoopWork() throws BuildException
	{
		try
		{
			if (!getHBaseAdmin().isCatalogJanitorEnabled())
			{
				getHBaseAdmin().enableCatalogJanitor(true);
			}
		}
		catch (Exception e)
		{
			throw new BuildException("Failed to enable catalog janitor", e);
		}
	}
}
