package com.anttoolkit.maestro.tasks.instance;

import org.apache.tools.ant.*;

import com.maestro.xml.*;

import com.anttoolkit.maestro.common.*;

@Action("suspend-instances")
public class SuspendInstanceTask extends InstanceTask
{
	@Param("ignore-state")
	private boolean ignoreState;

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

		waitInstancesStateToChange("suspended");
	}
}
