package com.anttoolkit.general.tasks.loops;

import org.apache.tools.ant.*;

import com.anttoolkit.general.tasks.*;

public class WhileLoopTask extends IfTask
{
	private String iterationProperty;

	private boolean isRoot = true;

	public void setIterationProperty(String property)
	{
		this.iterationProperty = property;
	}

	public void doWork()
			throws BuildException
	{
		this.setEcho(false);

		if (!isRoot)
		{
			super.doWork();
			return;
		}

		long i = 0;

		while (true)
		{
			this.reconfigure();
			WhileLoopTask iteration = (WhileLoopTask)this.getWrapper().getProxy();
			iteration.isRoot = false;

			if (iterationProperty != null)
			{
				this.setPropertyThreadSafe(iterationProperty, Long.toString(i));
			}

			iteration.perform();

			if (!iteration.wasExecuted())
			{
				return;
			}

			if (iterationProperty != null)
			{
				i++;
			}
		}
	}
}
