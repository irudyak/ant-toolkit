package com.anttoolkit.zookeeper.tasks;

import java.util.*;

import org.apache.tools.ant.*;

import com.anttoolkit.general.tasks.*;
import com.anttoolkit.zookeeper.types.*;
import com.anttoolkit.zookeeper.common.*;

public class ZookeeperSessionTask
		extends GenericTask
		implements TaskContainer
{
	private ZookeeperConfig config;
	private String connectionString;
	private int timeout = -1;
	private List<Task> tasks = new LinkedList<Task>();

	public void setZookeeperConfig(String ref)
	{
		Object obj = getReference(ref);
		if (!(obj instanceof ZookeeperConfig))
		{
			throw new IllegalArgumentException("Incorrect ZooKeeper config '" + ref + "' specified");
		}

		this.config = (ZookeeperConfig)obj;
	}

	public void setConnectionString(String connection)
	{
		this.connectionString = connection;
	}

	public void setTimeout(int timeout)
	{
		this.timeout = timeout;
	}

	@Override
	public void doWork() throws BuildException
	{
		ZookeeperSessionManager.setCurrentContext(getConfig());

		try
		{
			for (Task task : tasks)
			{
				task.perform();
			}
		}
		finally
		{
			ZookeeperSessionManager.releaseCurrentContext();
		}
	}

	@Override
	public void addTask(Task task)
	{
		tasks.add(task);
	}

	private ZookeeperConfig getConfig()
	{
		if (config != null)
		{
			config.init(this);
			return config;
		}

		if (connectionString == null || connectionString.trim().isEmpty() || timeout == -1)
		{
			throw new BuildException("Either ZooKeeper config or connection string and timeout should be specified");
		}

		config = new ZookeeperConfig();
		config.setConnectionString(connectionString);
		config.setTimeout(timeout);
		config.init(this);

		return config;
	}
}
