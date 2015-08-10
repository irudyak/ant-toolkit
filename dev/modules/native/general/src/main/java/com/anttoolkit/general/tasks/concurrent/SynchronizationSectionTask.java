package com.anttoolkit.general.tasks.concurrent;

import java.util.*;

import com.anttoolkit.general.tasks.GenericTask;
import org.apache.tools.ant.*;

import com.anttoolkit.general.tasks.concurrent.util.*;

public class SynchronizationSectionTask
		extends GenericTask
		implements TaskContainer
{
	private List<Task> tasks = new LinkedList<Task>();

	private String sectionName;
	private boolean echo = false;

	public void setSectionName(String name)
	{
		sectionName = name;
	}

	public void setEcho(boolean echo)
	{
		this.echo = echo;
	}

	public void addTask(Task task)
	{
		try
		{
			Task newTask = null;

			if (task instanceof UnknownElement)
			{
				newTask = ((UnknownElement)task).copy(getProject());
				newTask.setProject(getProject());
			}
			else
			{
				newTask = (Task)task.clone();
			}

			tasks.add(newTask);
		}
		catch (Throwable e)
		{
			throw new BuildException("Failed to clone task", e);
		}
	}

	public void doWork() throws BuildException
	{
		if (sectionName == null)
		{
			throw new BuildException("Synchronization section name should be specified");
		}

		Thread thread = Thread.currentThread();
		TasksThread tasksThread = thread instanceof TasksThread ? (TasksThread)thread : null;

		Object synchSectionObject = ThreadManager.getSynchronizationSectionObject(sectionName);

		if (echo)
		{
			if (tasksThread != null)
			{
				tasksThread.log("Trying to enter synchronization section '" + sectionName + "'");
			}
			else
			{
				this.log("Trying to enter synchronization section '" + sectionName + "'");
			}
		}

		synchronized (synchSectionObject)
		{
			if (echo)
			{
				if (tasksThread != null)
				{
					tasksThread.log("Entered synchronization section '" + sectionName + "'");
				}
				else
				{
					this.log("Entered synchronization section '" + sectionName + "'");
				}
			}

			for (Task task : tasks)
			{
				task.perform();
			}

			if (echo)
			{
				if (tasksThread != null)
				{
					tasksThread.log("Left synchronization section '" + sectionName + "'");
				}
				else
				{
					this.log("Left synchronization section '" + sectionName + "'");
				}
			}
		}
	}
}
