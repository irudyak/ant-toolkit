package com.anttoolkit.zookeeper.tasks;

import java.util.*;

import org.apache.tools.ant.*;
import org.apache.zookeeper.CreateMode;
import org.apache.zookeeper.Op;
import org.apache.zookeeper.ZooDefs;

public class TransactionTask
		extends GenericZookeeperTask
		implements TaskContainer
{
	private List<Task> tasks = new LinkedList<Task>();

	@Override
	public void doWork() throws BuildException
	{
		getZookeeperSession().startTransaction();

		for (Task task : tasks)
		{
			task.perform();
		}

		getZookeeperSession().commitTransaction();
	}

	@Override
	public void addTask(Task task)
	{
		tasks.add(task);
	}
}
