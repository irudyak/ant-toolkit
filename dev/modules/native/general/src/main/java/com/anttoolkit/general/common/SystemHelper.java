package com.anttoolkit.general.common;

import org.apache.tools.ant.*;

import java.net.*;

public class SystemHelper
{
	public static final String lineSeparator = System.getProperty("line.separator");
	public static final String doubleLineSeparator = lineSeparator + lineSeparator;
	public static final String osName = System.getProperty("os.name");
	public static final String osUser = System.getProperty("user.name");
	public static final boolean isWindows = osName.toLowerCase().contains("win");
	public static final boolean isMac = osName.toLowerCase().contains("mac");
	public static final boolean isUnix = osName.toLowerCase().contains("nux");
	public static final String hostName;
	public static final String hostIp;

	static
	{
		try
		{
			InetAddress address = InetAddress.getLocalHost();
			hostName = address.getHostName();
			hostIp = address.getHostAddress();
		}
		catch (UnknownHostException e)
		{
			throw new BuildException("Failed to get host/ip of current computer", e);
		}
	}
}
