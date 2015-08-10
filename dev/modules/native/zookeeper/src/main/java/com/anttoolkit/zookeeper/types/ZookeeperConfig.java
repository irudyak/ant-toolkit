package com.anttoolkit.zookeeper.types;

import java.util.*;

import com.anttoolkit.general.tasks.*;

public class ZookeeperConfig
{
	private String connection;
	private int timeout = -1;
	private List<AuthInfo> authInfoList = new LinkedList<AuthInfo>();

	public void setConnectionString(String connection)
	{
		this.connection = connection;
	}

	public String getConnectionString()
	{
		return connection;
	}

	public void setTimeout(int timeout)
	{
		this.timeout = timeout;
	}

	public int getTimeout()
	{
		return timeout;
	}

	public void addConfiguredAuthInfo(AuthInfo info)
	{
		authInfoList.add(info);
	}

	public List<AuthInfo> getAuthInfo()
	{
		return Collections.unmodifiableList(authInfoList);
	}

	public void init(GenericTask task)
	{
		for (AuthInfo info : authInfoList)
		{
			info.init(task);
		}
	}
}
