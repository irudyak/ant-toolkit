package com.anttoolkit.mongodb.tasks;

import java.util.*;

import org.apache.tools.ant.*;

public class MongoSessionTask
		extends GenericMongoTask
		implements TaskContainer
{
	private List<Task> tasks = new LinkedList<Task>();

	@Override
	public void doWork() throws BuildException
	{
		for (Task task : tasks)
		{
			task.perform();
		}
	}

	@Override
	public void addTask(Task task)
	{
		tasks.add(task);
	}

	protected void validate()
	{
		if (!isMongoConfigSpecified())
		{
			throw new BuildException("Mongo config should be specified");
		}
	}
}
