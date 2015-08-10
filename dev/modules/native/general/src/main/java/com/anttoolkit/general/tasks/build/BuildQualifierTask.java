package com.anttoolkit.general.tasks.build;

import java.util.*;

import org.apache.tools.ant.*;

import com.anttoolkit.general.tasks.*;
import com.anttoolkit.general.tasks.build.util.*;

public class BuildQualifierTask
		extends GenericTask
		implements TaskContainer

{
	private List<Task> tasks = new LinkedList<Task>();
	private boolean hasEmptyBody = true;
	private String qualifier;

	public void setQualifier(String qualifier)
	{
		this.qualifier = qualifier;
	}

	public void addText(String text)
	{
		hasEmptyBody = false;
	}

	@Override
	public void doWork() throws BuildException
	{
		BuildContextManager.setContextQualifier(qualifier);

		try
		{
			for (Task task : tasks)
			{
				task.perform();
			}
		}
		finally
		{
			if (!tasks.isEmpty() || !hasEmptyBody)
			{
				BuildContextManager.releaseContextQualifier();
			}
		}
	}

	@Override
	public void addTask(Task task)
	{
		tasks.add(task);
	}

	@Override
	protected void validate()
	{
		if (qualifier == null || qualifier.trim().isEmpty())
		{
			throw new BuildException("Build qualifier should be specified");
		}
	}
}
