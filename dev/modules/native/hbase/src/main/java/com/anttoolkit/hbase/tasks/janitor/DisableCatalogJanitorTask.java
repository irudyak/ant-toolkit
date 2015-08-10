package com.anttoolkit.hbase.tasks.janitor;

import org.apache.tools.ant.*;

import com.anttoolkit.hbase.tasks.*;

public class DisableCatalogJanitorTask extends GenericHBaseTask
{
	@Override
	protected void doHadoopWork() throws BuildException
	{
		try
		{
			if (getHBaseAdmin().isCatalogJanitorEnabled())
			{
				getHBaseAdmin().enableCatalogJanitor(false);
			}
		}
		catch (Exception e)
		{
			throw new BuildException("Failed to disable catalog janitor", e);
		}
	}
}
