package com.anttoolkit.hadoop.tasks.hadoop;

import java.util.*;

import org.apache.tools.ant.*;

public class HadoopSessionTask
	extends GenericHadoopTask
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
		for (Task task : tasks)
		{
			task.perform();
		}
	}
}
