package com.anttoolkit.hbase.tasks.server;

import org.apache.tools.ant.*;

import com.anttoolkit.hbase.tasks.*;

public class CheckServerExistsTask extends GenericHBaseTask
{
	private String server;
	private String property;

	public void setServer(String name)
	{
		server = name;
	}

	public void setProperty(String property)
	{
		this.property = property;
	}

	@Override
	protected void doHadoopWork() throws BuildException
	{
		try
		{
			getServer(server);
		}
		catch (BuildException e)
		{
			setPropertyThreadSafe(property, Boolean.TRUE.toString());
			return;
		}

		setPropertyThreadSafe(property, Boolean.FALSE.toString());
	}

	@Override
	protected void hadoopValidate()
	{
		if (server == null || server.trim().isEmpty())
		{
			throw new BuildException("Server name should be specified");
		}

		if (property == null || property.trim().isEmpty())
		{
			throw new BuildException("Property name should be specified");
		}
	}
}
