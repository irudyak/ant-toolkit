package com.anttoolkit.hbase.tasks;

import java.io.*;

import org.apache.tools.ant.*;

public class StopMasterTask extends GenericHBaseTask
{
	@Override
	protected void doHadoopWork() throws BuildException
	{
		try
		{
			getHBaseAdmin().stopMaster();
		}
		catch (IOException e)
		{
			throw new BuildException("Failed to stop HBase master");
		}
	}
}
