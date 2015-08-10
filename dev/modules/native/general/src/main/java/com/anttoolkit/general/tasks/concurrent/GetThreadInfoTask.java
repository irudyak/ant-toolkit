package com.anttoolkit.general.tasks.concurrent;

import com.anttoolkit.general.tasks.GenericTask;
import com.anttoolkit.general.tasks.concurrent.util.TasksThread;
import org.apache.tools.ant.*;

public class GetThreadInfoTask
		extends GenericTask
{
	private String nameProperty = null;
	private String idProperty = null;
	private String logFileProperty = null;

	public void setNameProperty(String property)
	{
		nameProperty = property;
	}

	public void setIdProperty(String property)
	{
		idProperty = property;
	}

	public void setLogFileProperty(String property)
	{
		logFileProperty = property;
	}

	public void doWork() throws BuildException
	{
		Thread thread = Thread.currentThread();

		if (nameProperty != null)
		{
			setPropertyThreadSafe(nameProperty, thread.getName());
		}

		if (idProperty != null)
		{
			setPropertyThreadSafe(idProperty, Long.toString(thread.getId()));
		}

		if (logFileProperty != null &&
			thread instanceof TasksThread &&
			((TasksThread)thread).getLogFile() != null)
		{
			setPropertyThreadSafe(logFileProperty, ((TasksThread) thread).getLogFile());
		}
	}
}
