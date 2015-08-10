package com.anttoolkit.zookeeper.tasks;

import org.apache.tools.ant.*;

public class CheckNodeTask extends GenericZookeeperTask
{
	private String node;
	private int version = -1;

	public void setNode(String node)
	{
		this.node = node;
	}

	public void setVersion(int version)
	{
		this.version = version;
	}

	@Override
	public void doWork() throws BuildException
	{
		getZookeeperSession().check(node, version);
	}

	@Override
	protected void validate()
	{
		if (node == null || node.trim().isEmpty())
		{
			throw new BuildException("Node name should be specified");
		}

		if (version < 0)
		{
			throw new BuildException("Correct node version should be specified");
		}
	}
}
