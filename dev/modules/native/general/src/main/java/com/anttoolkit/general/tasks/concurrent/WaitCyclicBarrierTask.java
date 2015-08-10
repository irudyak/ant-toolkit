package com.anttoolkit.general.tasks.concurrent;

import java.util.concurrent.*;

import org.apache.tools.ant.*;

import com.anttoolkit.general.tasks.*;
import com.anttoolkit.general.tasks.concurrent.util.*;

public class WaitCyclicBarrierTask
		extends GenericTask
{
	private String barrierName = null;
	private boolean echo = true;

	public void setName(String name)
	{
		barrierName = name;
	}

	public void setEcho(boolean echo)
	{
		this.echo = echo;
	}

	public void doWork() throws BuildException
	{
		if (barrierName == null)
		{
			throw new BuildException("Cyclic barrier name should be specified");
		}

		Thread thread = Thread.currentThread();
		TasksThread tasksThread = thread instanceof TasksThread ? (TasksThread)thread : null;

		CyclicBarrier barrier = ThreadManager.getCyclicBarrier(barrierName);
		if (barrier == null)
		{
			throw new BuildException("There are no cyclic barrier with name: " + barrierName);
		}

		try
		{
			if (echo)
			{
				if (tasksThread != null)
				{
					tasksThread.log("Waiting for barrier '" + barrierName + "'");
				}
				else
				{
					this.log("Waiting for barrier '" + barrierName + "'");
				}
			}

			barrier.await();

			if (echo)
			{
				if (tasksThread != null)
				{
					tasksThread.log("Waiting for barrier '" + barrierName + "' completed");
				}
				else
				{
					this.log("Waiting for barrier '" + barrierName + "' completed");
				}
			}
		}
		catch (InterruptedException e)
		{
			throw new BuildException("Waiting for cyclic barrier '" + barrierName + "' was interrupted", e);
		}
		catch (BrokenBarrierException ex)
		{
			throw new BuildException("Cyclic barrier '" + barrierName + "' is broken", ex);
		}
	}
}
