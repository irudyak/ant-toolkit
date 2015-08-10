package com.anttoolkit.general.tasks.orchestration.util;

import java.text.*;

public abstract class OrchestrationStep
{
	public static final String STEP_INFO_SEPARATOR = ";";

	private long startTime = -1;
	private long finishTime = -1;
	private String executionSummary;
	private String threadGroup;

	protected OrchestrationStep(String threadGroup)
	{
		this.threadGroup = threadGroup == null ? "" : threadGroup;
	}

	protected OrchestrationStep(String threadGroup, String executionSummary)
	{
		this.threadGroup = threadGroup == null ? "" : threadGroup;
		this.executionSummary = executionSummary;
	}

	public abstract String getUniqueId();

	public String getThreadLog(String logFolder, String threadName)
	{
		if (logFolder == null || logFolder.trim().isEmpty())
		{
			return null;
		}

		String folder = logFolder.trim().replace("\\", "/");
		folder = folder.endsWith("/") ? folder : folder + "/";

		return folder + threadName + ".log";
	}

	public String getThreadGroup()
	{
		return threadGroup;
	}

	public boolean equals(Object obj)
	{
		return obj instanceof OrchestrationStep &&
			((OrchestrationStep) obj).getUniqueId().equals(this.getUniqueId());
		}

	public int hashCode()
	{
		return getUniqueId().hashCode();
	}

	public boolean executedInCurrentSession()
	{
		return executionSummary == null && startTime != -1 && finishTime != -1;
	}

	public void setStartTime()
	{
		if (executionSummary == null)
		{
			startTime = System.currentTimeMillis();
		}
	}

	public void setFinishTime()
	{
		if (executionSummary == null)
		{
			finishTime = System.currentTimeMillis();
		}
	}

	public String serialize(DateFormat format)
	{
		return getUniqueId() + STEP_INFO_SEPARATOR + getExecutionSummary(format);
	}

	public String getExecutionSummary(DateFormat format)
	{
		if (executionSummary != null)
		{
			return executionSummary;
		}

		if (startTime == -1)
		{
			return "";
		}

		if (finishTime == -1)
		{
			return " START=[" + format.format(startTime) + "]";
		}

		return " START=[" + format.format(startTime) + "] " +
				"FINISH=[" + format.format(finishTime) + "] " +
				"DURATION=[" + ((finishTime - startTime) / 1000) + " sec]";
	}
}
