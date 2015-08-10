package com.anttoolkit.hbase.tasks;

import java.io.*;

import org.apache.tools.ant.*;

public class ShutdownTask extends GenericHBaseTask
{
	@Override
	protected void doHadoopWork() throws BuildException
	{
		try
		{
			getHBaseAdmin().shutdown();
		}
		catch (IOException e)
		{
			throw new BuildException("Failed to shutdown HBase cluster");
		}
	}
}
