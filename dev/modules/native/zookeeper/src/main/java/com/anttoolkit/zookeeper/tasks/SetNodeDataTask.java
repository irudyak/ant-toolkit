package com.anttoolkit.zookeeper.tasks;

import java.nio.charset.*;

import org.apache.tools.ant.*;

public class SetNodeDataTask extends GenericZookeeperTask
{
	private String node;
	private String data;
	private String reference;
	private int version = -1;

	public void setNode(String node)
	{
		this.node = node;
	}

	public void setData(String data)
	{
		this.data = data;
	}

	public void setDataReference(String reference)
	{
		this.reference = reference;
	}

	public void setVersion(int version)
	{
		this.version = version;
	}

	@Override
	public void doWork() throws BuildException
	{
		if (data != null)
		{
			getZookeeperSession().setData(node, version, data.getBytes(Charset.forName("UTF-8")));
			return;
		}

		Object obj = this.getReference(reference);
		if (obj == null || obj instanceof byte[])
		{
			getZookeeperSession().setData(node, version, (byte[])obj);
		}
		else
		{
			getZookeeperSession().setData(node, version, obj.toString().getBytes(Charset.forName("UTF-8")));
		}
	}

	@Override
	protected void validate()
	{
		if (node == null || node.trim().isEmpty())
		{
			throw new BuildException("Node name should be specified");
		}

		if ((data == null && reference == null) ||
			(data != null && reference != null))
		{
			throw new BuildException("Either data or data reference should be specified");
		}

		if (!getZookeeperSession().exists(node))
		{
			throw new BuildException("Specified ZooKeeper node doesn't exist: " + node);
		}
	}
}
