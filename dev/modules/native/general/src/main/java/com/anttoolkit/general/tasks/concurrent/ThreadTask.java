package com.anttoolkit.general.tasks.concurrent;

import java.util.*;

import org.apache.tools.ant.*;

import com.anttoolkit.general.tasks.*;
import com.anttoolkit.general.tasks.concurrent.util.*;

public class ThreadTask
		extends GenericTask
		implements TaskContainer
{
	private List<Task> tasks = new LinkedList<Task>();

	private String threadName = null;
	private String threadGroup = null;
	private String logFile = null;
	private boolean echo = true;
	private String associatedValues = null;
	private String separator = ",";
	private boolean forbidNewThreadsOnFailure = true;

	public void setName(String name)
	{
		threadName = name;
	}

	public void setEcho(boolean echo)
	{
		this.echo = echo;
	}

	public void setForbidNewThreadsOnFailure(boolean forbid)
	{
		forbidNewThreadsOnFailure = forbid;
	}

	public void setAssociatedValues(String value)
	{
		associatedValues = value;
	}

	public void setSeparator(String separator)
	{
		this.separator = separator;
	}

	public void setGroup(String group)
	{
		threadGroup = group;
	}

	public void setLogFile(String file)
	{
		logFile = file;
	}

	public void doWork() throws BuildException
	{
		String[] associatedValues = this.associatedValues == null || this.associatedValues.trim().length() == 0 ? null : this.associatedValues.split(separator, -1);
		ThreadManager.startThread(this, threadName, associatedValues, threadGroup, logFile, tasks, null, echo, forbidNewThreadsOnFailure);
	}

	public void addTask(Task task)
	{
		try
		{
			Task newTask;

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
}
