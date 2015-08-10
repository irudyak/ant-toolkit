package com.anttoolkit.zookeeper.tasks;

import java.util.*;

import org.apache.tools.ant.*;

public class ChildrenLoopTask
		extends GenericZookeeperTask
		implements TaskContainer
{
	private String node;
	private boolean recursive = false;
	private String property;
	private List<Task> tasks = new LinkedList<Task>();

	public void setNode(String node)
	{
		this.node = node;
	}

	public void setRecursive(boolean recursive)
	{
		this.recursive = recursive;
	}

	public void setProperty(String property)
	{
		this.property = property;
	}

	@Override
	public void doWork() throws BuildException
	{
		List<String> children = getZookeeperSession().getChildren(node, recursive);

		for (String node : children)
		{
			this.setPropertyThreadSafe(property, node);

			for (Task task : tasks)
			{
				task.perform();
			}
		}
	}

	@Override
	public void addTask(Task task)
	{
		tasks.add(task);
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
			throw new BuildException("Property to store node name should be specified");
		}
	}
}
