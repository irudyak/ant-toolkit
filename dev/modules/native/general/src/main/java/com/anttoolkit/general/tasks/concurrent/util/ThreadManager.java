package com.anttoolkit.general.tasks.concurrent.util;

import java.util.*;
import java.util.concurrent.*;

import org.apache.tools.ant.*;

import com.anttoolkit.general.loggers.*;
import com.anttoolkit.general.tasks.orchestration.util.*;

public class ThreadManager
{
	private static boolean isThreadsCreationForbidden = false;
	private static Map<String, List<Thread>> threadsMap = new ConcurrentHashMap<String, List<Thread>>();
	private static Map<String, CyclicBarrier> cyclicBarriers = new HashMap<String, CyclicBarrier>();
	private static Map<String, Object> synchSections = new HashMap<String, Object>();

	public static Thread startThread(Task rootTask, String threadName, String threadGroup,
								   String logFile, Task task, OrchestrationStep step,
								   boolean echo, boolean forbidNewThreadsOnFailure)
	{
		LinkedList<Task> tasks = new LinkedList<Task>();
		tasks.add(task);

		return startThread(rootTask, threadName, null, threadGroup, logFile, tasks, step, echo, forbidNewThreadsOnFailure);
	}

	public static Thread startThread(Task rootTask, String threadName, String[] associatedValues, String threadGroup,
								   String logFile, List<Task> tasks, OrchestrationStep step,
								   boolean echo, boolean forbidNewThreadsOnFailure)
	{
		try
		{
			if (logFile != null && !logFile.trim().isEmpty() &&
				!logFile.contains("/") && !logFile.contains("\\") &&
				rootTask.getProject().getProperty(GenericLogger.LOGS_DIR) != null)
			{
				logFile = rootTask.getProject().getProperty(GenericLogger.LOGS_DIR) + "/" + logFile;
			}

			return startThread(threadName, associatedValues, threadGroup, logFile, tasks, step, echo, forbidNewThreadsOnFailure);
		}
		catch (ThreadsCreationForbiddenException e)
		{
			String msg = "[WARNING] Creation of new thread '" + threadName + "' was forbidden, due to the failures of previously created threads";
			rootTask.log(msg, Project.MSG_ERR);

			//if it is the main thread - wait for all other threads to complete
			if (!(Thread.currentThread() instanceof TasksThread))
			{
				ThreadManager.waitAllThreadsToComplete(rootTask, true);
			}

			throw new BuildException(msg);
		}
	}

	public static void waitThreadsToComplete(Task rootTask, String threadGroup, boolean failOnAny, boolean forbidNewThreadsOnFailure)
	{
		//wait for all threads
		if (threadGroup == null)
		{
			waitAllThreadsToComplete(rootTask, true);
			return;
		}

		int failedThreadsCount = waitThreadsToComplete(threadGroup, rootTask);

		if (!failOnAny || failedThreadsCount == 0)
		{
			return;
		}

		//forbid creation of new threads
		if (forbidNewThreadsOnFailure)
		{
			forbidNewThreadsCreation();
		}

		if (Thread.currentThread() instanceof TasksThread)	//not main Ant thread
		{
			throw new BuildException(failedThreadsCount + " threads are failed in group '" + threadGroup + "'");
		}

		waitAllThreadsToComplete(rootTask, forbidNewThreadsOnFailure);

		throw new BuildException(failedThreadsCount + " threads are failed in group '" + threadGroup + "'");
	}

	public static synchronized void forbidNewThreadsCreation()
	{
		isThreadsCreationForbidden = true;
	}

	public static boolean hasAnyThreadGroups()
	{
		Map<String, List<Thread>> threadsMap = getThreadsMap();

		//noinspection SynchronizationOnLocalVariableOrMethodParameter
		synchronized (threadsMap)
		{
			return !threadsMap.keySet().isEmpty();
		}
	}

	public static String getAnyThreadGroup()
	{
		Map<String, List<Thread>> threadsMap = getThreadsMap();

		//noinspection SynchronizationOnLocalVariableOrMethodParameter
		synchronized (threadsMap)
		{
			if (threadsMap.keySet().isEmpty())
			{
				return null;
			}

			Iterator<String> iter = threadsMap.keySet().iterator();

			return iter.hasNext() ? iter.next() : null;
		}
	}

	public static List<Thread> getAllRunningThreads()
	{
		List<Thread> runningThreads = new LinkedList<Thread>();

		Map<String, List<Thread>> threadsMap = getThreadsMap();

		//noinspection SynchronizationOnLocalVariableOrMethodParameter
		synchronized (threadsMap)
		{
			if (threadsMap.isEmpty())
			{
				return runningThreads;
			}

			for (List<Thread> threads : threadsMap.values())
			{
				for (Thread thread : threads)
				{
					if (thread.getState() != Thread.State.NEW &&
						thread.getState() != Thread.State.TERMINATED)
					{
						runningThreads.add(thread);
					}
				}
			}

			return runningThreads;
		}
	}

	public static int waitForCompletion(List<Thread> threads, long milliseconds)
	{
		if (threads == null || threads.isEmpty())
		{
			return 0;
		}

		for (Thread thread : threads)
		{
			try
			{
				thread.join(milliseconds);
			}
			catch (InterruptedException e) {}
		}

		int failedCount = 0;

		for (Thread thread : threads)
		{
			failedCount += thread.isInterrupted() ? 1 : 0;
		}

		return failedCount;
	}

	public static List<Thread> removeThreadGroup(String threadGroup)
	{
		if (threadGroup == null || threadGroup.trim().length() == 0)
		{
			return null;
		}

		Map<String, List<Thread>> threadsMap = getThreadsMap();

		//noinspection SynchronizationOnLocalVariableOrMethodParameter
		synchronized (threadsMap)
		{
			return threadsMap.containsKey(threadGroup) ? threadsMap.remove(threadGroup) : null;
		}
	}

	public static void createCyclicBarrier(String barrierName, int parties)
	{
		Map<String, CyclicBarrier> barriers = getCyclicBarriersMap();

		//noinspection SynchronizationOnLocalVariableOrMethodParameter
		synchronized (barriers)
		{
			CyclicBarrier barrier = barriers.get(barrierName);
			if (barrier != null && barrier.getNumberWaiting() != 0)
			{
				throw new BuildException("Cyclic barrier with name '" + barrierName + "' already exists and " +
					barrier.getNumberWaiting() + " threads waiting waiting for it");
			}

			CyclicBarrier previousBarrier = barriers.put(barrierName, new CyclicBarrier(parties));
			if (previousBarrier != null)
			{
				previousBarrier.reset();
			}
		}
	}

	public static CyclicBarrier getCyclicBarrier(String barrierName)
	{
		if (barrierName == null)
		{
			throw new BuildException("Cyclic barrier name couldn't be null");
		}

		Map<String, CyclicBarrier> barriers = getCyclicBarriersMap();

		//noinspection SynchronizationOnLocalVariableOrMethodParameter
		synchronized (barriers)
		{
			if (barriers.containsKey(barrierName))
			{
				return barriers.get(barrierName);
			}

			throw new BuildException("Cyclic barrier with name '" + barrierName + "' doesn't exist");
		}
	}

	public static Object getSynchronizationSectionObject(String sectionName)
	{
		if (sectionName == null)
		{
			return null;
		}

		Map<String, Object> synchSections = getSynchronizationSectionsMap();

		//noinspection SynchronizationOnLocalVariableOrMethodParameter
		synchronized (synchSections)
		{
			if (!synchSections.containsKey(sectionName))
			{
				synchSections.put(sectionName, sectionName);
			}

			return synchSections.get(sectionName);
		}
	}

	private static Thread startThread(String threadName, String[] associatedValues, String threadGroup,
								   String logFile, List<Task> tasks, OrchestrationStep step,
								   boolean echo, boolean forbidNewThreadsOnFailure)
			throws ThreadsCreationForbiddenException
	{
		if (tasks == null || tasks.isEmpty())
		{
			return null;
		}

		if (threadGroup == null || threadGroup.trim().length() == 0)
		{
			throw new BuildException("Thread group is not specified");
		}

		Map<String, List<Thread>> threadsMap = getThreadsMap();

		//noinspection SynchronizationOnLocalVariableOrMethodParameter
		synchronized (threadsMap)
		{
			if (isThreadsCreationForbidden())
			{
				throw new ThreadsCreationForbiddenException();
			}

			TasksThread thread = new TasksThread(threadName, threadGroup, associatedValues, logFile, tasks, step, echo, forbidNewThreadsOnFailure);

			List<Thread> threads = threadsMap.get(threadGroup);
			if (threads == null)
			{
				threads = Collections.synchronizedList(new LinkedList<Thread>());
				threadsMap.put(threadGroup, threads);
			}

			threads.add(thread);

			thread.start();

			return thread;
		}
	}

	private static int waitThreadsToComplete(String threadsGroup, Task task)
	{
		List<Thread> threads = ThreadManager.removeThreadGroup(threadsGroup);
		if (threads == null || threads.isEmpty())
		{
			task.log(buildLogMessagePrefix(task.getProject(), "START-THREADS-WAIT -> FINISH-THREADS-WAIT", threadsGroup) + " threads: 0");
			return 0;
		}

		task.log(buildLogMessagePrefix(task.getProject(), "START-THREADS-WAIT", threadsGroup) + " threads: " + threads.size());

		int failedCount = waitForCompletion(threads, 0);

		String msg = buildLogMessagePrefix(task.getProject(), "FINISH-THREADS-WAIT", threadsGroup) +
						" threads: " + threads.size() +
						", success: " + Integer.toString(threads.size() - failedCount) +
						", failed: " + failedCount + getFailedThreadsInfo(threads);

		task.log(msg, failedCount == 0 ? Project.MSG_INFO : Project.MSG_ERR);

		return failedCount;
	}

	private static void waitAllThreadsToComplete(Task task, boolean forbidNewThreadsCreation)
	{
		if (forbidNewThreadsCreation)
		{
			forbidNewThreadsCreation();
		}

		Map<String, List<Thread>> threadsMap = getThreadsMap();

		//noinspection SynchronizationOnLocalVariableOrMethodParameter
		synchronized (threadsMap)
		{
			List<Thread> threads = getAllRunningThreads();
			int threadsCount = threads.size();

			StringBuilder builder = new StringBuilder();

			if (threadsCount > 0)
			{
				for (Thread thread : threads)
				{
					if (builder.length() > 0)
					{
						builder.append(", ");
					}

					builder.append(thread.getName());
				}
			}

			task.log("[START-WAITING-ALL-RUNNING-THREADS] " + threadsCount + " threads: " + builder.toString(), Project.MSG_INFO);

			while (hasAnyThreadGroups())
			{
				String group = getAnyThreadGroup();
				if (group == null)
				{
					continue;
				}

				waitThreadsToComplete(group, task);
			}

			task.log("[FINISHED-WAITING-ALL-RUNNING-THREADS] " + threadsCount + " threads: " + builder.toString(), Project.MSG_INFO);
		}
	}

	private static Map<String, List<Thread>> getThreadsMap()
	{
		return threadsMap;
	}

	private static Map<String, Object> getSynchronizationSectionsMap()
	{
		return synchSections;
	}

	private static Map<String, CyclicBarrier> getCyclicBarriersMap()
	{
		return cyclicBarriers;
	}

	private static synchronized boolean isThreadsCreationForbidden()
	{
		return isThreadsCreationForbidden;
	}

	private static String buildLogMessagePrefix(Project project, String prefix, String threadGroup)
	{
		boolean isThreadAwareLoggerRegistered = ThreadAwareLogger.isThreadAwareLoggerRegistered(project);

		return (!isThreadAwareLoggerRegistered ? "<" + Thread.currentThread().getName() + "> " : "") +
			"[" + prefix + (isThreadsCreationForbidden() ? ", FORCED-BY-FAILURE" : "") + "] " +
			"threadsGroup: " + threadGroup + ", ";
	}

	private static String getFailedThreadsInfo(List<Thread> threads)
	{
		if (threads == null || threads.isEmpty())
		{
			return "";
		}

		Collections.sort(threads, ThreadsComparator.INSTANCE);

		StringBuilder builder = new StringBuilder();

		int i = 0;
		for (Thread thread : threads)
		{
			if (!thread.isInterrupted())
			{
				continue;
			}

			if (i != 0)
			{
				builder.append(", ");
			}

			builder.append(thread.toString());

			i++;
		}

		return builder.length() == 0 ? "" : "; " + builder.toString();
	}
}
