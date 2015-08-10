package com.anttoolkit.hbase.tasks;

import java.util.*;

import org.apache.tools.ant.*;

import com.anttoolkit.hbase.common.*;

public class ResourcePoolContainerTask
		extends GenericHBaseTask
		implements TaskContainer
{
	private List<Task> tasks = new LinkedList<Task>();

	@Override
	public void addTask(Task task)
	{
		tasks.add(task);
	}

	@Override
	protected void doHadoopWork() throws BuildException
	{
		HBaseResourcesManager.setCurrentContext(getConfiguration());

		try
		{
			for (Task task : tasks)
			{
				task.perform();
			}
		}
		finally
		{
			HBaseResourcesManager.releaseCurrentContext();
		}
	}
}
