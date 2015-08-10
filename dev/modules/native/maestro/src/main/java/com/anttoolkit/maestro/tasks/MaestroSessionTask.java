package com.anttoolkit.maestro.tasks;

import java.util.*;

import org.apache.tools.ant.*;

public class MaestroSessionTask
		extends GenericMaestroTask
		implements TaskContainer
{
	private List<Task> tasks = new LinkedList<Task>();

	@Override
	public void addTask(Task task)
	{
		tasks.add(task);
	}

	@Override
	public void doWork() throws BuildException
	{
		for (Task task : tasks)
		{
			task.perform();
		}
	}
}
