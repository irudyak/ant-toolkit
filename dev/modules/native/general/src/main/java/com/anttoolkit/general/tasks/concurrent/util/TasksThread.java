package com.anttoolkit.general.tasks.concurrent.util;

import java.io.*;
import java.util.*;

import org.apache.tools.ant.*;
import org.apache.tools.ant.property.LocalProperties;
import org.apache.tools.ant.util.*;

import com.anttoolkit.general.loggers.*;
import com.anttoolkit.general.entities.*;
import com.anttoolkit.general.entities.EntityManager.*;
import com.anttoolkit.general.tasks.orchestration.util.*;

public class TasksThread extends Thread implements Comparable, StepProcessor
{
	private String threadGroup = null;
	private String logFile = null;
	private List<Task> tasks = null;
	private Throwable error = null;
	private boolean echo = true;
	private String[] associatedValues = null;
	private boolean forbidNewThreadsOnFailure = true;

	private boolean isThreadAwareLoggerRegistered = false;

	private List<Map<EntityType, Map<String, Object[]>>> scopeStack = null;

	private Orchestration orchestration = null;
	private OrchestrationStep orcStep = null;

	TasksThread(String threadName, String threadGroup, String[] associatedValues, String logFile, List<Task> tasks, OrchestrationStep step, boolean echo, boolean forbidNewThreadsOnFailure)
	{
		super(threadName);

		if (threadGroup == null || threadGroup.trim().isEmpty())
		{
			throw new IllegalArgumentException("Thread group can't be empty");
		}

		this.threadGroup = threadGroup;

		if (tasks == null || tasks.isEmpty())
		{
			throw new IllegalArgumentException("No tasks specified for thread: " + threadName);
		}

		this.logFile = logFile;
		this.tasks = tasks;
		this.echo = echo;
		this.associatedValues = associatedValues;
		this.forbidNewThreadsOnFailure = forbidNewThreadsOnFailure;
		isThreadAwareLoggerRegistered = ThreadAwareLogger.isThreadAwareLoggerRegistered(this.tasks.get(0).getProject());

		scopeStack = EntityManager.createScopeStackForNewThread();

		orcStep = step;
		orchestration = OrchestrationManager.getCurrentOrchestration();
	}

	public String getGroup()
	{
		return threadGroup;
	}

	@Override
	public void run()
	{
		LocalProperties.get(getProject()).copy();

		if (orchestration != null)
		{
			OrchestrationManager.joinOrchestration(orchestration);
		}

		if (orcStep != null && orchestration != null)
		{
			orchestration.processStep(orcStep, this);
			return;
		}

		processStep(orchestration, null);
	}

	@Override
	public void processStep(Orchestration orc, OrchestrationStep step)
	{
		Task currentTask = null;

		try
		{
			EntityManager.initScopeStackForNewThread(scopeStack);
			scopeStack = null;

			long startTime = System.currentTimeMillis();

			log("Thread execution started");

			for (Task task : tasks)
			{
				currentTask = task;
				task.perform();
			}

			log("Thread execution completed, duration: " + getDurationInfo(startTime));
		}
		catch (Throwable e)
		{
			if (forbidNewThreadsOnFailure)
			{
				ThreadManager.forbidNewThreadsCreation();
			}

			error = e;
			log(currentTask, "Thread exception occured:", e);

			if (orc != null && step != null)
			{
				if (e instanceof BuildException)
				{
					throw (BuildException)e;
				}

				throw new BuildException(e);
			}
		}
		finally
		{
			scopeStack = null;
			EntityManager.destroyScopeStackForCompletedThread();
		}
	}

	public boolean isFailed()
	{
		return error != null;
	}

	public boolean isInterrupted()
	{
		return isFailed() || super.isInterrupted();
	}

	public boolean showEcho()
	{
		return echo;
	}

	public Throwable getError()
	{
		return error;
	}

	public String getLogFile()
	{
		return logFile;
	}

	public boolean hasLogFile()
	{
		return logFile != null;
	}

	public String[] getAssociatedValues()
	{
		return associatedValues;
	}

	public String toString()
	{
		return getThreadDisplayName();
	}

	public int compareTo(Object obj)
	{
		return this.toString().compareTo(obj.toString());
	}

	public synchronized void writeToLogFile(String message)
	{
		if (logFile == null || logFile.trim().length() == 0)
		{
			return;
		}

		PrintWriter writer = null;

		try
		{
			writer = new PrintWriter(new FileOutputStream(logFile, true));
			writer.println(message);
		}
		catch (Throwable e)
		{
		}
		finally
		{
			if (writer != null)
			{
				try
				{
					writer.close();
				}
				catch (Throwable e) {}
			}
		}
	}

	public void log(String message)
	{
		getProject().log(getLoggingPrefix() + message);

		if (!isThreadAwareLoggerRegistered)
		{
			writeToLogFile(message);
		}
	}

	private void log(Task task, String message, Throwable e)
	{
		if (task == null)
		{
			getProject().log(getLoggingPrefix() + message, e, Project.MSG_ERR);
		}
		else
		{
			getProject().log(task, getLoggingPrefix() + message, e, Project.MSG_ERR);
		}

		if (!isThreadAwareLoggerRegistered)
		{
			writeToLogFile(message + StringUtils.getStackTrace(e));
		}
	}

	private String getThreadDisplayName()
	{
		if (getName() != null && getName().trim().length() != 0)
		{
			return getName();
		}

		return Long.toString(getId());
	}

	private String getDurationInfo(long startTime)
	{
		long milliseconds = System.currentTimeMillis() - startTime;
		if (milliseconds < 1000)
		{
			return milliseconds + " milliseconds";
		}

		long seconds = milliseconds / 1000;
		return seconds + " seconds";
	}

	private Project getProject()
	{
		return tasks.get(0).getProject();
	}

	private String getLoggingPrefix()
	{
		return isThreadAwareLoggerRegistered ? "" : "<" + getThreadDisplayName() + "> ";
	}
}
