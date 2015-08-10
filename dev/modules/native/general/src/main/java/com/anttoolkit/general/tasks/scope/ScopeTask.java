package com.anttoolkit.general.tasks.scope;

import java.util.*;

import com.anttoolkit.general.tasks.GenericTask;
import org.apache.tools.ant.*;

import com.anttoolkit.general.entities.*;

public class ScopeTask
		extends GenericTask
		implements TaskContainer

{
	private List<Task> tasks = new LinkedList<Task>();

	@Override
	public void doWork() throws BuildException
	{
		EntityManager.enterLocalScope();

		try
		{
			for (Task task : tasks)
			{
				task.perform();
			}
		}
		finally
		{
			try
			{
				EntityManager.leaveLocalScope();
			}
			catch (LocalScopeAbsentException e)
			{
				//noinspection ThrowFromFinallyBlock
				throw new BuildException("No local scope to exit", e);
			}
		}
	}

	@Override
	public void addTask(Task task)
	{
		tasks.add(task);
	}
}
