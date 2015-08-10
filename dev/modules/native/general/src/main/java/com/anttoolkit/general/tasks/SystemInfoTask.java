package com.anttoolkit.general.tasks;

import org.apache.tools.ant.*;

import com.anttoolkit.general.common.*;

public class SystemInfoTask extends GenericTask
{
	private String userProp;
	private String osProp;
	private String hostProp;
	private String ipProp;

	public void setUserProperty(String property)
	{
		this.userProp = property;
	}

	public void setOsProperty(String property)
	{
		this.osProp = property;
	}

	public void setHostProperty(String property)
	{
		this.hostProp = property;
	}

	public void setIpProperty(String property)
	{
		this.ipProp = property;
	}

	@Override
	public void doWork() throws BuildException
	{
		if (userProp != null)
		{
			this.setPropertyThreadSafe(userProp, SystemHelper.osUser);
		}

		if (osProp != null)
		{
			this.setPropertyThreadSafe(osProp, SystemHelper.osName);
		}

		if (hostProp != null)
		{
			this.setPropertyThreadSafe(hostProp, SystemHelper.hostName);
		}

		if (ipProp != null)
		{
			this.setPropertyThreadSafe(ipProp, SystemHelper.hostIp);
		}
	}
}
