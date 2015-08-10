package com.anttoolkit.general.tasks.orchestration.util;

public class WorkStep extends OrchestrationStep
{
	private String name;

	public WorkStep(String name, String threadGroup)
	{
		super(threadGroup);

		if (name == null || name.trim().length() == 0)
		{
			throw new IllegalArgumentException("Step name can't be empty");
		}

		this.name = name.trim();
	}

	private WorkStep(String name, String threadGroup, String executionSummary)
	{
		super(threadGroup, executionSummary);

		if (name == null || name.trim().length() == 0)
		{
			throw new IllegalArgumentException("Step name can't be empty");
		}

		this.name = name.trim();
	}

	public static WorkStep deserialize(String stepInfo)
	{
		if (stepInfo == null || stepInfo.trim().length() == 0)
		{
			throw new IllegalArgumentException("Failed to create new step, cause step specification is empty");
		}

		String[] chunks = stepInfo.split(STEP_INFO_SEPARATOR, -1);
		if (chunks.length != 3)
		{
			throw new IllegalArgumentException("Incorrect step specification: " + stepInfo);
		}

		return new WorkStep(chunks[0].trim(), chunks[1].trim(), chunks[2]);
	}

	public String getUniqueId()
	{
		return name + STEP_INFO_SEPARATOR + getThreadGroup();
	}

	public String getName()
	{
		return name;
	}

	public String toString()
	{
		return "WORK_STEP: " + (getThreadGroup() == null || getThreadGroup().trim().isEmpty() ? name : name + STEP_INFO_SEPARATOR + getThreadGroup());
	}
}
