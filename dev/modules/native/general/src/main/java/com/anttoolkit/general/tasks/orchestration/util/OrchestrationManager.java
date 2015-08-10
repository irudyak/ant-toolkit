package com.anttoolkit.general.tasks.orchestration.util;

import java.text.*;
import java.util.*;

import org.apache.tools.ant.*;

public class OrchestrationManager
{
	private static List<Orchestration> orchestrations = new LinkedList<Orchestration>();
	private static ThreadLocal<Orchestration> currentOrc = new ThreadLocal<Orchestration>();

	public static Orchestration startNewOrchestration(Project project, String name, String configFile,
					  String progressLog, List<String> statusLogs, String threadsLogFolder, DateFormat dateFormat,
					  OrchestrationLogger logger, boolean forbidNewThreadsOnFailure)
	{
		if (currentOrc.get() != null && !currentOrc.get().isCompleted())
		{
			throw new IllegalStateException("Can't start orchestration '" + name + "' for the thread '" +
					Thread.currentThread().getName() + "' cause it is already executing inside '" +
					currentOrc.get().getName() + "' orchestration. Nested orchestrations not supported.");
		}

		//noinspection SynchronizationOnLocalVariableOrMethodParameter
		synchronized (orchestrations)
		{
			validateNewOrchestration(orchestrations, name, progressLog, statusLogs);

			Orchestration orc = new Orchestration(project, name, configFile, progressLog, statusLogs, threadsLogFolder, dateFormat, logger, forbidNewThreadsOnFailure);

			currentOrc.set(orc);
			orchestrations.add(orc);

			orc.start();

			return orc;
		}
	}

	public static void joinOrchestration(Orchestration orc)
	{
		if (orc == null)
		{
			return;
		}

		if (currentOrc.get() != null)
		{
			throw new IllegalStateException("Can't join thread '" + Thread.currentThread().getName() + "' to " +
					"'" + orc.getName() + "' orchestration cause it is already executing inside '" +
					currentOrc.get().getName() + "' orchestration");
		}

		//noinspection SynchronizationOnLocalVariableOrMethodParameter
		synchronized (orchestrations)
		{
			boolean started = checkOrchestrationAlreadyStarted(orc, orchestrations);
			if (!started)
			{
				throw new BuildException("Failed to join thread '" + Thread.currentThread().getName() +
						"' to '" + orc.getName() + "' orchestration, cause it wasn't previously started");
			}

			currentOrc.set(orc);
			orc.attach(Thread.currentThread());
		}
	}

	public static Orchestration getCurrentOrchestration()
	{
		return currentOrc.get();
	}

	private static void validateNewOrchestration(List<Orchestration> orcList, String name, String progressLog, List<String> statusLogs)
	{
		for (Orchestration _orc : orcList)
		{
			if (_orc.getName().equals(name))
			{
				throw new BuildException("Can't start orchestration '" + name + "', cause orchestration with the same name already exist");
			}

			if (_orc.getProgressLog().equals(progressLog))
			{
				throw new BuildException("Can't start orchestration '" + name + "', cause orchestration '" + _orc.getName() + "' already has the same progress log");
			}

			if (_orc.getStatusLogs() == null || _orc.getStatusLogs().isEmpty() ||
				statusLogs == null || statusLogs.isEmpty())
			{
				return;
			}

			for (String statusLog : statusLogs)
			{
				if (_orc.getStatusLogs().contains(statusLog))
				{
					throw new BuildException("Can't start orchestration '" + name + "', cause orchestration '" + _orc.getName() + "' already has the same status log: " + statusLog);
				}
			}
		}
	}

	private static boolean checkOrchestrationAlreadyStarted(Orchestration orc, List<Orchestration> orcList)
	{
		for (Orchestration _orc : orcList)
		{
			if (_orc.equals(orc))
			{
				return true;
			}
		}

		return false;
	}
}
