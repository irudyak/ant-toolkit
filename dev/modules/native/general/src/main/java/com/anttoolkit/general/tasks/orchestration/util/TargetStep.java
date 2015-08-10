package com.anttoolkit.general.tasks.orchestration.util;

import org.apache.tools.ant.*;

import com.anttoolkit.general.tasks.*;

import java.io.File;

public class TargetStep extends OrchestrationStep
{
	public static final String SYSTEM_WAIT_THREADS_STEP = "SYSTEM-WAIT-THREADS";

	private int serialNumber;
	private String antFile;
	private String antFileResolved;
	private String target;
	private String logFile;
	private String logFileResolved;

	public static TargetStep createFromConfigurationInfo(Project project, int serialNumber, String stepInfo)
	{
		if (stepInfo == null || stepInfo.trim().length() == 0)
		{
			throw new IllegalArgumentException("Failed to create new step, cause step specification is empty");
		}

		String[] chunks = stepInfo.split(STEP_INFO_SEPARATOR, -1);
		if (chunks.length == 0)
		{
			throw new IllegalArgumentException("Incorrect step specification: " + stepInfo);
		}

		TargetStep step = new TargetStep(chunks.length > 2 ? chunks[2].trim() : "");

		step.serialNumber = serialNumber;
		step.target = resolveTarget(chunks[0].trim(), stepInfo);
		step.antFile = chunks.length > 1 ? chunks[1].trim() : "";
		step.antFileResolved = step.antFile.length() == 0 ? step.antFile : resolveAntFile(project, step.antFile, stepInfo);
		step.logFile = chunks.length > 3 ? chunks[3].trim() : "";
		step.logFileResolved = step.logFile.length() == 0 ? step.logFile : resolveLogFile(project, step.logFile);

		return step;
	}

	public static TargetStep deserialize(Project project, String stepInfo)
	{
		if (stepInfo == null || stepInfo.trim().length() == 0)
		{
			throw new IllegalArgumentException("Failed to create new step, cause step specification is empty");
		}

		String[] chunks = stepInfo.split(STEP_INFO_SEPARATOR, -1);
		if (chunks.length != 5)
		{
			throw new IllegalArgumentException("Incorrect step specification: " + stepInfo);
		}

		TargetStep step = new TargetStep(chunks[3].trim(), chunks[4]);

		try
		{
			step.serialNumber = Integer.parseInt(chunks[0].trim());
		}
		catch (NumberFormatException e)
		{
			throw new IllegalArgumentException("Incorrect step specification: " + stepInfo);
		}

		step.target = resolveTarget(chunks[1].trim(), stepInfo);
		step.antFile = chunks[2].trim();
		step.antFileResolved = resolveAntFile(project, step.antFile, stepInfo);

		return step;
	}

	protected TargetStep(String threadGroup)
	{
		super(threadGroup);
	}

	private TargetStep(String threadGroup, String executionSummary)
	{
		super(threadGroup, executionSummary);
	}

	public int getSerialNumber()
	{
		return serialNumber;
	}

	public String getTarget()
	{
		return target;
	}

	public String getAntFile()
	{
		return antFileResolved;
	}

	public String getLogFile()
	{
		return logFileResolved;
	}

	public String getUniqueId()
	{
		return serialNumber + STEP_INFO_SEPARATOR +
				target + STEP_INFO_SEPARATOR +
				antFile + STEP_INFO_SEPARATOR +
				getThreadGroup();
	}

	public String toString()
	{
		StringBuilder msg = new StringBuilder();
		msg.append(serialNumber).append(STEP_INFO_SEPARATOR);
		msg.append(target);

		if (antFile != null && !antFile.trim().isEmpty())
		{
			msg.append(STEP_INFO_SEPARATOR).append(antFile);
		}

		if (getThreadGroup() != null && !getThreadGroup().trim().isEmpty())
		{
			msg.append(STEP_INFO_SEPARATOR).append(getThreadGroup());
		}

		return "TARGET_STEP: " + getUniqueId();
	}

	public boolean isSystemStep()
	{
		return SYSTEM_WAIT_THREADS_STEP.equals(target);
	}

	private static String resolveAntFile(Project project, String file, String stepInfo)
	{
		String resolvedFile = file == null || file.length() == 0 ? "" : GenericTask.getFileFullPath(project, file);

		if (resolvedFile.length() != 0)
		{
			File _file = new File(resolvedFile);
			if (!_file.exists() || _file.isDirectory())
			{
				throw new IllegalArgumentException("Incorrect Ant file specified for the step: " + stepInfo);
			}
		}

		return resolvedFile;
	}

	private static String resolveLogFile(Project project, String file)
	{
		return file == null || file.length() == 0 ? "" : GenericTask.getFileFullPath(project, file);
	}

	private static String resolveTarget(String target, String stepInfo)
	{
		if (target == null || target.length() == 0)
		{
			throw new IllegalArgumentException("Target is empty in the step specification: " + stepInfo);
		}

		return target;
	}
}
