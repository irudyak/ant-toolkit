package com.anttoolkit.general.tasks.orchestration;

import java.io.*;
import java.text.*;
import java.util.*;

import com.anttoolkit.general.loggers.GenericLogger;
import org.apache.tools.ant.*;
import org.apache.tools.ant.taskdefs.*;

import com.anttoolkit.general.tasks.*;
import com.anttoolkit.general.tasks.concurrent.util.*;
import com.anttoolkit.general.tasks.orchestration.util.*;

public class OrchestrationTask
	extends GenericTask
	implements TaskContainer, OrchestrationLogger, StepProcessor
{
	private List<Task> tasks = new LinkedList<Task>();

	private String name;
	private String configFile;
	private String progressLog;
	private List<String> statusLogs = new LinkedList<String>();
	private String threadsLogFolder;
	private boolean forbidNewThreadsOnFailure = true;
	private String timeFormat = "MM/dd/yyyy hh:mm:ss aaa";
	private String locale = Locale.US.toString();
	private DateFormat dateFormat;

	public void setName(String name)
	{
		this.name = name;
	}

	public void setConfigFile(String file)
	{
		configFile = this.getFileFullPath(file);
	}

	public void setProgressLog(String file)
	{
		progressLog = this.getFileFullPath(file);
	}

	public void setStatusLogs(String files)
	{
		if (files == null || files.trim().isEmpty())
		{
			statusLogs.clear();
			return;
		}

		String[] chunks = files.split(",", -1);

		for (String chunk : chunks)
		{
			statusLogs.add(this.getFileFullPath(chunk.trim()));
		}
	}

	public void setTimeFormat(String format)
	{
		timeFormat = format;
	}

	public void setLocale(String locale)
	{
		this.locale = locale;
	}

	public void setThreadsLogFolder(String folder)
	{
		if (folder == null || folder.trim().isEmpty())
		{
			threadsLogFolder = null;
			return;
		}

		threadsLogFolder = this.getFileFullPath(folder);

		File file = new File(threadsLogFolder);
		if (!file.exists() || !file.isDirectory())
		{
			throw new IllegalArgumentException("Incorrect folder specified for orchestration '" + name + "' thread logs: " + threadsLogFolder);
		}
	}

	public void setForbidNewThreadsOnFailure(boolean forbid)
	{
		forbidNewThreadsOnFailure = forbid;
	}

	@Override
	public void doWork() throws BuildException
	{
		if (name == null)
		{
			throw new BuildException("Orchestration name couldn't be empty");
		}

		try
		{
			dateFormat = new SimpleDateFormat(timeFormat, new Locale(locale));
		}
		catch (Throwable e)
		{
			throw new BuildException("Incorrect time format/locale specified for orchestration '" + name + "'", e);
		}

		if (threadsLogFolder == null &&
			this.getProject().getProperty(GenericLogger.LOGS_DIR) != null)
		{
			threadsLogFolder = this.getFileFullPath(this.getProject().getProperty(GenericLogger.LOGS_DIR));
		}

		Orchestration orc = OrchestrationManager.startNewOrchestration(getProject(), name, configFile, progressLog, statusLogs, threadsLogFolder, dateFormat, this, forbidNewThreadsOnFailure);

		try
		{
			executeOrchestration(orc);
		}
		finally
		{
			orc.waitToComplete(this);
		}

		if (orc.isFailed())
		{
			throw new BuildException("Orchestration '" + name + "' failed");
		}
	}

	@Override
	public void addTask(Task task)
	{
		tasks.add(task);
	}

	@Override
	public void logOrchestrationEvent(String msg)
	{
		this.getProject().log(msg);
	}

	@Override
	public void processStep(Orchestration orc, OrchestrationStep step)
	{
		if (!(step instanceof TargetStep))
		{
			throw new IllegalStateException("Can't process step which is not an instance of TargetStep");
		}

		TargetStep targetStep = (TargetStep)step;

		if (targetStep.isSystemStep())
		{
			processSystemStep(targetStep);
		}
		else
		{
			processRegularStep(targetStep);
		}
	}

	private void executeOrchestration(Orchestration orc)
	{
		executeInternalSteps();
		executeConfiguredSteps(orc);
	}

	private void executeInternalSteps()
	{
		if (tasks.isEmpty())
		{
			return;
		}

		for (Task task : tasks)
		{
			task.perform();
		}
	}

	private void executeConfiguredSteps(Orchestration orc)
	{
		List<TargetStep> steps = orc.getConfiguredSteps();
		for (TargetStep step : steps)
		{
			//process step synchronously
			if (step.getThreadGroup() == null ||
				step.getThreadGroup().trim().isEmpty() ||
				step.isSystemStep())
			{
				orc.processStep(step, this);
				continue;
			}

			//process step asynchronously
			if (orc.checkStepAlreadyProcessed(step))
			{
				orc.notifyStepSkipped(step);
				continue;
			}

			String threadName = orc.hasOtherStepsHavingSameTarget(step) ?
								step.getTarget() + "_" + step.getSerialNumber() :
								step.getTarget();

			String threadLogFolder = orc.getThreadsLogFolder();
			if (threadLogFolder == null || threadLogFolder.trim().isEmpty())
			{
				threadLogFolder = this.getProject().getProperty(GenericLogger.LOGS_DIR);
			}

			String threadLog = step.getThreadLog(threadLogFolder, threadName);

			Thread thread = ThreadManager.startThread(this, threadName, step.getThreadGroup(), threadLog, createTargetStepTask(step), step, true, true);

			orc.attach(thread);
		}
	}

	private void processSystemStep(TargetStep targetStep)
	{
		if (!TargetStep.SYSTEM_WAIT_THREADS_STEP.equals(targetStep.getTarget()))
		{
			throw new IllegalArgumentException("Can't process unknown system step: " + targetStep.getUniqueId());
		}

		ThreadManager.waitThreadsToComplete(this, targetStep.getThreadGroup(), true, forbidNewThreadsOnFailure);
	}

	private void processRegularStep(TargetStep targetStep)
	{
		createTargetStepTask(targetStep).perform();
	}

	private Task createTargetStepTask(TargetStep targetStep)
	{
		if (targetStep.getAntFile() == null || targetStep.getAntFile().trim().isEmpty())
		{
			CallTargetTask task = new CallTargetTask();
			this.initDelegateTask(task);
			task.setTarget(targetStep.getTarget());

			return task;
		}

		Ant task = new Ant();
		this.initDelegateTask(task);
		task.setTarget(targetStep.getTarget());
		task.setAntfile(targetStep.getAntFile());
		task.setInheritAll(true);
		task.setInheritRefs(true);
		task.setUseNativeBasedir(true);

		if (targetStep.getLogFile() != null && !targetStep.getLogFile().trim().isEmpty())
		{
			task.setOutput(targetStep.getLogFile());
		}

		return task;
	}
}
