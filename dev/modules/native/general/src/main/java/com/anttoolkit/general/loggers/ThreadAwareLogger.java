package com.anttoolkit.general.loggers;

import java.io.*;
import java.text.*;
import java.util.*;

import org.apache.tools.ant.*;

import com.anttoolkit.general.tasks.*;
import com.anttoolkit.general.tasks.orchestration.util.*;
import com.anttoolkit.general.tasks.concurrent.util.*;

public class ThreadAwareLogger extends GenericLogger
{
	private static final ResourceBundle LOGGER_BUNDLE;
	private static final String LOGGER_PROPERTIES_FILE="anttoolkit-logger";
	private static final String SHOWN_MAIN_THREAD_PARAM = "show.main.thread";
	private static final String SHOWN_TIME_PARAM = "show.time";
	private static final String TIME_FORMAT_PARAM = "time.format";
	private static final String LOCALE_PARAM = "locale";

	static
	{
		ResourceBundle bundle = null;

		try
		{
			bundle = ResourceBundle.getBundle(LOGGER_PROPERTIES_FILE);
		}
		catch (MissingResourceException e)
		{
		}

		LOGGER_BUNDLE = bundle;
	}

	private boolean showMainThread = true;
	private boolean showTime = false;
	private SimpleDateFormat dateTimeFormatter = null;

	public static boolean isThreadAwareLoggerRegistered(Project project)
	{
		if (project == null ||
			project.getBuildListeners() == null ||
			project.getBuildListeners().isEmpty())
		{
			return false;
		}

		Vector listeners = project.getBuildListeners();
		for (int i = 0; i < listeners.size(); i++)
		{
			if (listeners.get(i) instanceof ThreadAwareLogger)
			{
				return true;
			}
		}

		return false;
	}

	public ThreadAwareLogger()
	{
		super();

		if (LOGGER_BUNDLE == null)
		{
			return;
		}

		try
		{
			showMainThread = Boolean.parseBoolean(LOGGER_BUNDLE.getString(SHOWN_MAIN_THREAD_PARAM));
		}
		catch (MissingResourceException e) {}

		try
		{
			showTime = Boolean.parseBoolean(LOGGER_BUNDLE.getString(SHOWN_TIME_PARAM));
		}
		catch (MissingResourceException e) {}

		String timeFormat = "MM/dd/yyyy HH:mm:ss";

		try
		{
			timeFormat = LOGGER_BUNDLE.getString(TIME_FORMAT_PARAM);
		}
		catch (MissingResourceException e) {}

		Locale locale = Locale.ENGLISH;

		try
		{
			String _locale = LOGGER_BUNDLE.getString(LOCALE_PARAM);
			String[] locales = _locale != null ? _locale.split(",", -1) : null;

			locale = locales == null ? Locale.ENGLISH :
				locales.length == 1 ? new Locale(locales[0]) :
						new Locale(locales[0], locales[1]);
		}
		catch (MissingResourceException e) {}

		if (showTime)
		{
			dateTimeFormatter = new SimpleDateFormat(timeFormat, locale);
		}
	}

	protected String getMessagePrefix(BuildEvent event)
	{
		Thread currentThread = Thread.currentThread();
		boolean isUnregisteredThread = !(currentThread instanceof TasksThread);
		boolean isMainThread = isUnregisteredThread && currentThread.getName().equalsIgnoreCase("main");

		if (isMainThread && !showMainThread)
		{
			return "";
		}

		StringBuilder builder = new StringBuilder();

		if (showTime)
		{
			builder.append(dateTimeFormatter.format(new Date())).append(" ");
		}

		if (!isUnregisteredThread || isMainThread)
		{
			builder.append("<").append(currentThread.getName()).append("> ");
		}
		else
		{
			builder.append("<").append(currentThread.getName()).append(" #").append(currentThread.getId()).append("> ");
		}

		return builder.toString();
	}

	protected void printMessage(final Project project, final String message, final PrintStream stream, final int priority)
	{
    	printMessage(message, stream, priority);

		if (!(Thread.currentThread() instanceof TasksThread) &&
			!Thread.currentThread().getName().equalsIgnoreCase("main"))
		{
			writeToUnregisteredThreadLog(message, project);
		}
 	}

	protected void printMessage(final String message, final PrintStream stream, final int priority)
	{
    	super.printMessage(message, stream, priority);

		if (Thread.currentThread() instanceof TasksThread)
		{
			((TasksThread)Thread.currentThread()).writeToLogFile(message);
		}
 	}

	private String getUnregisteredThreadsLogFolder(Project project)
	{
		Orchestration orc = OrchestrationManager.getCurrentOrchestration();

		String logFolder = orc != null ?
				orc.getThreadsLogFolder() :
				project.getProperty(LOGS_DIR);

		if (logFolder == null || logFolder.trim().length() == 0)
		{
			return null;
		}

		logFolder = logFolder.trim();

		logFolder = GenericTask.getFileFullPath(project, logFolder);
		logFolder = logFolder.replace("\\", "/");

		return logFolder.endsWith("/") ? logFolder : logFolder + "/";
	}

	private void writeToUnregisteredThreadLog(String message, Project project)
	{
		String logFolder = getUnregisteredThreadsLogFolder(project);
		if (logFolder == null)
		{
			return;
		}

		PrintWriter writer = null;

		try
		{
			String logFile = logFolder + Thread.currentThread().getId() + ".log";
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
}
