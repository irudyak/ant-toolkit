package com.anttoolkit.general.tasks;

import org.apache.tools.ant.*;

public class CallTargetTask
		extends GenericTask
{
	private String target;

	public void setTarget(String target)
	{
		this.target = target;
	}

	@Override
	public void doWork() throws BuildException
	{
		String[] targets = target.split(",", -1);

		for (String targetToExecute : targets)
		{
			this.getProject().executeTarget(targetToExecute);
		}
	}

	protected void validate()
	{
		if (target == null)
		{
			throw new BuildException("No targets specified");
		}
	}
}
