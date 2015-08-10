package com.anttoolkit.maestro.tasks.instance;

import org.apache.tools.ant.*;

import com.maestro.xml.*;

import com.anttoolkit.maestro.common.*;

@Action("stop-instances")
public class StopInstanceTask extends InstanceTask
{
	@Param
	private boolean force;

	@Param("ignore-state")
	private boolean ignoreState;

	public void setForce(boolean force)
	{
		this.force = force;
	}

	public void setIgnoreState(boolean ignore)
	{
		ignoreState = ignore;
	}

	@Override
	public void doWork() throws BuildException
	{
		XMLElement element = executeCommand();

		this.printResult(element);

		if (isAsync())
		{
			return;
		}

		waitInstancesStateToChange("stopped");
	}
}
