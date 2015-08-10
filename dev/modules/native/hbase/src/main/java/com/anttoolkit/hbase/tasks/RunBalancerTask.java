package com.anttoolkit.hbase.tasks;

import org.apache.tools.ant.*;

public class RunBalancerTask extends GenericHBaseTask
{
	@Override
	protected void doHadoopWork() throws BuildException
	{
		try
		{
			getHBaseAdmin().balancer();
		}
		catch (Throwable e)
		{
			throw e instanceof RuntimeException ?
					(RuntimeException)e :
					new BuildException("Failed to run balancer", e);
		}
	}
}
