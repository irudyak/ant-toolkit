package com.anttoolkit.zookeeper.tasks;

import java.util.*;

import org.apache.tools.ant.*;

import com.anttoolkit.general.tasks.collections.array.util.*;

public class GetChildrenTask extends GenericZookeeperTask
{
	private String array;
	private String node;
	private boolean recursive = false;

	public void setArray(String array)
	{
		this.array = array;
	}

	public void setNode(String node)
	{
		this.node = node;
	}

	public void setRecursive(boolean recursive)
	{
		this.recursive = recursive;
	}

	@Override
	public void doWork() throws BuildException
	{
		List<String> children = getZookeeperSession().getChildren(node, recursive);
		if (children == null || children.isEmpty())
		{
			return;
		}

		for (String node : children)
		{
			ArrayManager.add(array, node);
		}
	}

	@Override
	protected void validate()
	{
		if (node == null || node.trim().isEmpty())
		{
			throw new BuildException("Node name should be specified");
		}

		if (array == null || array.trim().isEmpty())
		{
			throw new BuildException("Array name should be specified");
		}

		if (!ArrayManager.exists(array))
		{
			throw new BuildException("Specified array '" + array + "' doesn't exist");
		}
	}
}
