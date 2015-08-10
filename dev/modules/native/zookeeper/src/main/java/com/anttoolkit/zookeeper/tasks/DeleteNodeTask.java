package com.anttoolkit.zookeeper.tasks;

import org.apache.tools.ant.*;

public class DeleteNodeTask extends GenericZookeeperTask
{
	private String node;
	private int version = -1;
	private boolean recursive = false;

	public void setNode(String node)
	{
		this.node = node;
	}

	public void setVersion(int version)
	{
		this.version = version;
	}

	public void setRecursive(boolean recursive)
	{
		this.recursive = recursive;
	}

	@Override
	public void doWork() throws BuildException
	{
		this.getZookeeperSession().delete(node, recursive, version);
	}

	@Override
	protected void validate()
	{
		if (node == null || node.trim().isEmpty())
		{
			throw new BuildException("Node name to delete should be specified");
		}
	}
}
