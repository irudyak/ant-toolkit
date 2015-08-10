package com.anttoolkit.general.tasks.orchestration;

import java.util.*;

import org.apache.tools.ant.*;

import com.anttoolkit.general.tasks.*;
import com.anttoolkit.general.tasks.orchestration.util.*;
import com.anttoolkit.general.tasks.concurrent.util.*;

public class WorkStepTask
	extends GenericTask
	implements TaskContainer, StepProcessor
{
	private String name;
	private String threadGroup;
	private List<Task> tasks = new LinkedList<Task>();

	public void setName(String name)
	{
		this.name = name;
	}

	public void setThreadGroup(String group)
	{
		threadGroup = group;
	}

	@Override
	public void doWork() throws BuildException
	{
		Orchestration orc = OrchestrationManager.getCurrentOrchestration();
		WorkStep step = new WorkStep(name, threadGroup);

		//process step synchronously
		if (threadGroup == null || threadGroup.trim().isEmpty())
		{
			orc.processStep(step, this);
			return;
		}

		//process step asynchronously
		if (orc.checkStepAlreadyProcessed(step))
		{
			orc.notifyStepSkipped(step);
			return;
		}

		String threadLog = step.getThreadLog(orc.getThreadsLogFolder(), step.getName());
		Thread thread = ThreadManager.startThread(this, step.getName(), null, threadGroup, threadLog, tasks, step, true, true);

		orc.attach(thread);
	}

	@Override
	public void addTask(Task task)
	{
		try
		{
			Task newTask = task;

			//clone tasks if they should be executed in a separate thread
			if (threadGroup != null && !threadGroup.trim().isEmpty())
			{
				if (task instanceof UnknownElement)
				{
					newTask = ((UnknownElement)task).copy(getProject());
					newTask.setProject(getProject());
				}
				else
				{
					newTask = (Task)task.clone();
				}
			}

			tasks.add(newTask);
		}
		catch (Throwable e)
		{
			throw new BuildException("Failed to clone task", e);
		}
	}

	@Override
	public void processStep(Orchestration orc, OrchestrationStep step)
	{
		for (Task task : tasks)
		{
			task.perform();
		}
	}

	protected void validate()
	{
		if (name == null)
		{
			throw new BuildException("The name for the part of work should be specified");
		}

		if (OrchestrationManager.getCurrentOrchestration() == null)
		{
			throw new BuildException("Can't start work step '" + name + "' cause it doesn't resides inside orchestration. " +
					"Work steps could be performed only inside orchestration.");
		}
	}
}
