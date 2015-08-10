package com.anttoolkit.general.tasks.loops;

import java.util.*;

import com.anttoolkit.general.tasks.GenericTask;
import org.apache.tools.ant.*;

public class ForLoopTask
		extends GenericTask
		implements TaskContainer
{
	private long from = -1;
	private long to = -1;
	private long increment = 1;
	private String iterationProperty;

	private List<Task> tasks = new LinkedList<Task>();

	public void addTask(Task task)
	{
		tasks.add(task);
	}

	public void setFrom(long from)
	{
		this.from = from;
	}

	public void setTo(long to)
	{
		this.to = to;
	}

	public void setIncrement(long increment)
	{
		this.increment = increment;
	}

	public void setIterationProperty(String property)
	{
		this.iterationProperty = property;
	}

	public void doWork()
			throws BuildException
	{
		if ((increment > 0 && from > to) ||
			(increment < 0 && from < to))
		{
			return;
		}

		for (long i = from; increment > 0 ? i <= to : i >= to; i = i + increment)
		{
			if (iterationProperty != null)
			{
				this.setPropertyThreadSafe(iterationProperty, Long.toString(i));
			}

			for (Task task : tasks)
			{
				task.perform();
			}
		}
	}

	protected void validate()
	{
		if (increment == 0)
		{
			throw new BuildException("Increment couldn't be 0");
		}
	}
}
