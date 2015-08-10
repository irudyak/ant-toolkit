package com.anttoolkit.zookeeper.tasks;

import org.apache.tools.ant.*;

public class CheckNodeExistsTask extends GenericZookeeperTask
{
	private String node;
	private String property;

	public void setNode(String node)
	{
		this.node = node;
	}

	public void setProperty(String property)
	{
		this.property = property;
	}

	@Override
	public void doWork() throws BuildException
	{
		Boolean exists = this.getZookeeperSession().exists(node);
		this.setPropertyThreadSafe(property, exists.toString());
	}

	@Override
	protected void validate()
	{
		if (node == null || node.trim().isEmpty())
		{
			throw new BuildException("Node name should be specified");
		}

		if (property == null || property.trim().isEmpty())
		{
			throw new BuildException("Property name should be specified");
		}
	}
}
