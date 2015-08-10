package com.anttoolkit.general.tasks.orchestration.util;

import java.io.*;
import java.text.*;
import java.util.*;

import com.anttoolkit.general.tasks.concurrent.util.*;
import org.apache.tools.ant.*;

import com.anttoolkit.general.common.*;

public class Orchestration
{
	private String name;
	private String configFile;
	private String progressLog;
	private List<String> statusLogs;
	private DateFormat dateFormat = new SimpleDateFormat("MM/dd/yyyy hh:mm:ss aaa", Locale.US);
	private OrchestrationLogger logger;
	private String threadsLogFolder;
	private boolean forbidNewThreadsOnFailure = true;
	private long startTime = -1;
	private long finishTime = -1;

	private List<Thread> threads = new LinkedList<Thread>();

	private List<TargetStep> configuredSteps = new LinkedList<TargetStep>();

	private List<OrchestrationStep> completedSteps = new LinkedList<OrchestrationStep>();
	private List<OrchestrationStep> failedSteps = new LinkedList<OrchestrationStep>();
	private List<OrchestrationStep> runningSteps = new LinkedList<OrchestrationStep>();
	private List<OrchestrationStep> skippedSteps = new LinkedList<OrchestrationStep>();

	Orchestration(Project project, String name, String configFile,
				  String progressLog, List<String> statusLogs, String threadsLogFolder,
				  DateFormat dateFormat, OrchestrationLogger logger, boolean forbidNewThreadsOnFailure)
	{
		if (name == null || name.trim().length() == 0)
		{
			throw new IllegalArgumentException("Orchestration name can't be empty");
		}

		this.name = name.trim();
		this.configFile = configFile;
		this.progressLog = progressLog;
		this.statusLogs = statusLogs;
		this.dateFormat = dateFormat;
		this.logger = logger;
		this.threadsLogFolder = threadsLogFolder;
		this.forbidNewThreadsOnFailure = forbidNewThreadsOnFailure;

		if (logger != null)
		{
			String _configFile = configFile != null && !configFile.trim().isEmpty() ? configFile : "NONE";
			String _progressLog = progressLog != null && !progressLog.trim().isEmpty() ? progressLog : "NONE";
			String _statusLogs = "NONE";

			if (statusLogs != null && !statusLogs.isEmpty())
			{
				_statusLogs = "";
				for (String statusLog : statusLogs)
				{
					_statusLogs = !_statusLogs.isEmpty() ? _statusLogs + "," : _statusLogs;
					_statusLogs += statusLog;
				}
			}

			logger.logOrchestrationEvent("[ORCHESTRATION-CREATION] " + name +
					" CONFIG-FILE=[" + _configFile + "] " +
					" PROGRESS-LOG=[" + _progressLog + "] " +
					" STATUS-LOGS=[" + _statusLogs + "]");
		}

		loadConfiguredSteps(project);
		loadCompletedSteps(project);
	}

	public String getName()
	{
		return name;
	}

	public String getProgressLog()
	{
		return progressLog;
	}

	public List<String> getStatusLogs()
	{
		return statusLogs;
	}

	public DateFormat getDateFormat()
	{
		return dateFormat;
	}

	public String getThreadsLogFolder()
	{
		return threadsLogFolder;
	}

	public void processStep(OrchestrationStep step, StepProcessor processor)
	{
		if (checkStepAlreadyProcessed(step))
		{
			notifyStepSkipped(step);
			return;
		}

		synchronized (SynchronizationManager.getSynchronizationObject("orchestration", name + step.getUniqueId()))
		{
			if (checkStepAlreadyProcessed(step))
			{
				notifyStepSkipped(step);
				return;
			}

			try
			{
				notifyStepStarted(step);

				processor.processStep(this, step);

				notifyStepCompleted(step);
			}
			catch (Throwable e)
			{
				notifyStepFailed(step);

				if (e instanceof BuildException)
				{
					throw (BuildException)e;
				}

				throw new BuildException("Orchestration step failed: (" + name + "/" + step.getUniqueId() + ")", e);
			}
		}
	}

	public synchronized boolean checkStepAlreadyProcessed(OrchestrationStep step)
	{
		return completedSteps.contains(step) || failedSteps.contains(step) || runningSteps.contains(step);
	}

	public synchronized void notifyStepSkipped(OrchestrationStep step)
	{
		step.setFinishTime();

		skippedSteps.add(step);

		synchStatusLogs();

		logStepStatus(step, "[STEP-SKIPPED]");
	}

	public List<TargetStep> getConfiguredSteps()
	{
		return Collections.unmodifiableList(configuredSteps);
	}

	public boolean hasOtherStepsHavingSameTarget(TargetStep step)
	{
		for (TargetStep _step : configuredSteps)
		{
			if (_step.getTarget().equals(step.getTarget()) && !_step.equals(step))
			{
				return true;
			}
		}

		return false;
	}

	public void waitToComplete(Task task)
	{
		try
		{
			List<String> threadGroups = getRunningThreadGroups();
			for (String group : threadGroups)
			{
				ThreadManager.waitThreadsToComplete(task, group, true, forbidNewThreadsOnFailure);
			}
		}
		finally
		{
			finishTime = System.currentTimeMillis();

			if (logger != null)
			{
				long duration = (finishTime - startTime) / 1000;

				StringBuilder summary = new StringBuilder();
				summary.append(" START=[").append(dateFormat.format(startTime)).append("] ");
				summary.append("FINISH=[").append(dateFormat.format(finishTime)).append("] ");
				summary.append("DURATION=[").append(duration).append(" sec] ");
				summary.append("EXECUTED=").append(getExecutedStepsCount()).append(" ");
				summary.append("SKIPPED=").append(skippedSteps.size()).append(" ");
				summary.append("FAILED=").append(failedSteps.size()).append(" ");
				summary.append("DORMANT=").append(getDormantSteps().size());

				String event = failedSteps.isEmpty() ? "[ORCHESTRATION-COMPLETED] " : "[ORCHESTRATION-FAILED] ";

				logger.logOrchestrationEvent(event + name + summary.toString());
			}
		}
	}

	public boolean isCompleted()
	{
		return startTime != -1 && finishTime != -1;
	}

	public boolean isFailed()
	{
		return !failedSteps.isEmpty();
	}

	public void attach(Thread thread)
	{
		//noinspection SynchronizeOnNonFinalField
		synchronized (threads)
		{
			if (!threads.contains(thread))
			{
				threads.add(thread);
			}
		}
	}

	void start()
	{
		startTime = System.currentTimeMillis();

		if (logger == null)
		{
			return;
		}

		logger.logOrchestrationEvent("[ORCHESTRATION-STARTED] " + name + " START=[" + dateFormat.format(startTime) + "]");
	}

	private synchronized void notifyStepStarted(OrchestrationStep step)
	{
		if (checkStepAlreadyProcessed(step))
		{
			throw new IllegalStateException("Incorrect orchestration state, trying to start step which was already processed or currently running: (" + name + "/" + step.getUniqueId() + ")");
		}

		step.setStartTime();

		runningSteps.add(step);

		synchStatusLogs();

		logStepStatus(step, "[STEP-STARTED]");
	}

	private synchronized void notifyStepCompleted(OrchestrationStep step)
	{
		if (!runningSteps.contains(step))
		{
			throw new IllegalStateException("Incorrect orchestration state, trying to complete step which wasn't previously running: (" + name + "/" + step.getUniqueId() + ")");
		}

		if (completedSteps.contains(step) || skippedSteps.contains(step) || failedSteps.contains(step))
		{
			throw new IllegalStateException("Incorrect orchestration state, trying to complete step which was already processed: (" + name + "/" + step.getUniqueId() + ")");
		}

		step.setFinishTime();

		runningSteps.remove(step);
		completedSteps.add(step);

		synchProgressLog();
		synchStatusLogs();

		logStepStatus(step, "[STEP-COMPLETED]");
	}

	private synchronized void notifyStepFailed(OrchestrationStep step)
	{
		if (!runningSteps.contains(step))
		{
			throw new IllegalStateException("Incorrect orchestration state, trying to mark step which wasn't previously running as failed: (" + name + "/" + step.getUniqueId() + ")");
		}

		if (completedSteps.contains(step))
		{
			throw new IllegalStateException("Incorrect orchestration state, trying to mark step which was previously completed as failed: (" + name + "/" + step.getUniqueId() + ")");
		}

		if (skippedSteps.contains(step))
		{
			throw new IllegalStateException("Incorrect orchestration state, trying to mark step which was previously skipped as failed: (" + name + "/" + step.getUniqueId() + ")");
		}

		step.setFinishTime();

		runningSteps.remove(step);
		failedSteps.add(step);

		synchStatusLogs();

		logStepStatus(step, "[STEP-FAILED]");
	}

	private void synchProgressLog()
	{
		if (progressLog == null || completedSteps.isEmpty())
		{
			return;
		}

		PrintWriter writer = null;

		try
		{
			writer = new PrintWriter(new FileOutputStream(progressLog, false));

			for (OrchestrationStep step : completedSteps)
			{
				writer.println(step.serialize(dateFormat));
			}
		}
		catch (FileNotFoundException e)
		{
			throw new BuildException("Failed to write progress to progress log file " + progressLog, e);
		}
		finally
		{
			if (writer != null)
			{
				writer.close();
			}
		}
	}

	private void synchStatusLogs()
	{
		if (statusLogs == null || statusLogs.isEmpty())
		{
			return;
		}

		for (String statusLog : statusLogs)
		{
			synchStatusLog(statusLog);
		}
	}

	private void synchStatusLog(String statusLog)
	{
		PrintWriter writer = null;

		try
		{
			writer = new PrintWriter(new FileOutputStream(statusLog, false));

			if (runningSteps.size() != 0)
			{
				writer.println("[RUNNING]");
				for (OrchestrationStep step : runningSteps)
				{
					writer.println(step.serialize(dateFormat));
				}
				writer.println("");
			}

			if (completedSteps.size() != 0)
			{
				boolean anyStepsExecuted = false;
				for (OrchestrationStep step : completedSteps)
				{
					if (step.executedInCurrentSession())
					{
						anyStepsExecuted = true;
						break;
					}
				}

				if (anyStepsExecuted)
				{
					writer.println("[EXECUTED]");
					for (OrchestrationStep step : completedSteps)
					{
						if (step.executedInCurrentSession())
						{
							writer.println(step.serialize(dateFormat));
						}
					}
					writer.println("");
				}
			}

			if (skippedSteps.size() != 0)
			{
				writer.println("[SKIPPED]");
				for (OrchestrationStep step : skippedSteps)
				{
					writer.println(step.serialize(dateFormat));
				}
				writer.println("");
			}

			if (failedSteps.size() != 0)
			{
				writer.println("[FAILED]");
				for (OrchestrationStep step : failedSteps)
				{
					writer.println(step.serialize(dateFormat));
				}
				writer.println("");
			}

			List<OrchestrationStep> dormantSteps = getDormantSteps();
			if (dormantSteps != null && !dormantSteps.isEmpty())
			{
				writer.println("[DORMANT]");
				for (OrchestrationStep step : dormantSteps)
				{
					writer.println(step.serialize(dateFormat));
				}
				writer.println("");
			}
		}
		catch (FileNotFoundException e)
		{
			throw new BuildException("Failed to write status to status log file " + statusLog, e);
		}
		finally
		{
			if (writer != null)
			{
				writer.close();
			}
		}
	}

	private List<OrchestrationStep> getDormantSteps()
	{
		List<OrchestrationStep> dormantSteps = new LinkedList<OrchestrationStep>();

		if (configuredSteps.isEmpty())
		{
			return dormantSteps;
		}

		for (OrchestrationStep step : configuredSteps)
		{
			if (!completedSteps.contains(step) && !failedSteps.contains(step) &&
				!runningSteps.contains(step) && !skippedSteps.contains(step))
			{
				dormantSteps.add(step);
			}
		}

		return dormantSteps;
	}

	private void logStepStatus(OrchestrationStep step, String msg)
	{
		if (logger != null)
		{
			logger.logOrchestrationEvent(msg + " (" + name +
					"/" + step.toString() +
					")" + step.getExecutionSummary(dateFormat));
		}
	}

	private void loadConfiguredSteps(Project project)
	{
		if (configFile == null)
		{
			return;
		}

		if (logger != null)
		{
			logger.logOrchestrationEvent("[ORCHESTRATION-CONFIG-FILE-PROCESSING] " + name + " START=[" + dateFormat.format(System.currentTimeMillis()) + "]");
		}

		File file = new File(configFile);
		if (!file.exists() || file.isDirectory())
		{
			throw new BuildException("Incorrect configuration file was specified for '" + name + "' orchestration: " + configFile);
		}

		BufferedReader reader = null;

		try
		{
			int i = 0;

			reader = new BufferedReader(new FileReader(file));
			String line;

			while ((line = reader.readLine()) != null)
			{
				i++;

				//skip comments and blank lines
				if (line.trim().isEmpty() || line.trim().startsWith("#"))
				{
					continue;
				}

				configuredSteps.add(TargetStep.createFromConfigurationInfo(project, i, line));
			}
		}
		catch (IOException e)
		{
			throw new BuildException("Failed to read orchestration config file: " + configFile, e);
		}
		finally
		{
			if (reader != null)
			{
				try
				{
					reader.close();
				}
				catch (Throwable e) {}
			}
		}

		if (logger != null)
		{
			logger.logOrchestrationEvent("[ORCHESTRATION-CONFIG-FILE-PROCESSED] " + name + " STEPS=" + configuredSteps.size() + " FINISH=[" + dateFormat.format(System.currentTimeMillis()) + "]");
		}
	}

	private void loadCompletedSteps(Project project)
	{
		if (progressLog == null)
		{
			return;
		}

		if (logger != null)
		{
			logger.logOrchestrationEvent("[ORCHESTRATION-PROGRESS-FILE-PROCESSING] " + name + " START=[" + dateFormat.format(System.currentTimeMillis()) + "]");
		}

		File file = new File(progressLog);
		if (file.isDirectory())
		{
			throw new BuildException("Incorrect progress file was specified for '" + name + "' orchestration: " + progressLog + ". Actually this is a directory instead of file.");
		}

		BufferedReader reader = null;

		try
		{
			reader = new BufferedReader(new FileReader(file));
			String line;

			while ((line = reader.readLine()) != null)
			{
				//skip comments and blank lines
				if (line.trim().isEmpty() || line.trim().startsWith("#"))
				{
					continue;
				}

				completedSteps.add(deserializeStep(project, line));
			}
		}
		catch (FileNotFoundException e)
		{
			if (logger != null)
			{
				logger.logOrchestrationEvent("[ORCHESTRATION-PROGRESS-FILE-PROCESSED] " + name + " STEPS=0 (File absent) FINISH=[" + dateFormat.format(System.currentTimeMillis()) + "]");
			}

			return;
		}
		catch (IOException e)
		{
			throw new BuildException("Failed to read orchestration progress log: " + progressLog, e);
		}
		finally
		{
			if (reader != null)
			{
				try
				{
					reader.close();
				}
				catch (Throwable e) {}
			}
		}

		if (logger != null)
		{
			logger.logOrchestrationEvent("[ORCHESTRATION-PROGRESS-FILE-PROCESSED] " + name + " STEPS=" + completedSteps.size() + " FINISH=[" + dateFormat.format(System.currentTimeMillis()) + "]");
		}
	}

	private OrchestrationStep deserializeStep(Project project, String stepInfo)
	{
		try
		{
			return WorkStep.deserialize(stepInfo);
		}
		catch (IllegalArgumentException e) {}

		try
		{
			return TargetStep.deserialize(project, stepInfo);
		}
		catch (IllegalArgumentException e) {}

		throw new BuildException("Incorrect step specification in orchestration '" + name + "': " + stepInfo);
	}

	private List<String> getRunningThreadGroups()
	{
		List<String> groups = new LinkedList<String>();

		synchronized (threads)
		{
			for (Thread thread : threads)
			{
				if ((thread instanceof TasksThread) &&
					!thread.getState().equals(Thread.State.TERMINATED))
				{
					groups.add(((TasksThread)thread).getGroup());
				}
			}
		}

		return groups;
	}

	private synchronized int getExecutedStepsCount()
	{
		int count = 0;
		for (OrchestrationStep step : completedSteps)
		{
			if (step.executedInCurrentSession())
			{
				count++;
			}
		}

		return count;
	}
}
