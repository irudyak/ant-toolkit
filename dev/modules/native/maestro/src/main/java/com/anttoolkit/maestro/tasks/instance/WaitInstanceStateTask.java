package com.anttoolkit.maestro.tasks.instance;

import java.util.*;

import org.apache.tools.ant.*;

import com.maestro.xml.*;

import com.anttoolkit.maestro.tasks.*;
import com.anttoolkit.maestro.common.*;

@Action("describe-instances")
public class WaitInstanceStateTask extends CliCommandTask
{
	public static final long WAIT_TIME = 1200000;	//20 mins
	public static final long SLEEP_TIMEOUT = 60000;	//1min

	@Param
	private String instance;

	private String state;
	private long wait = WAIT_TIME;

	public void setInstance(String instance)
	{
		this.instance = instance;
	}

	public void setState(String state)
	{
		this.state = state;
	}

	public void setWait(long wait)
	{
		if (wait <= 0)
		{
			throw new BuildException("Incorrect wait time specified: " + wait);
		}

		this.wait = wait;
	}

	@Override
	public void doWork() throws BuildException
	{
		if (state.equals(getInstanceState()))
		{
			return;
		}

		long start = System.currentTimeMillis();

		while (System.currentTimeMillis() - start < wait)
		{
			try
			{
				if (state.equals(getInstanceState()))
				{
					return;
				}

				Thread.sleep(SLEEP_TIMEOUT);
			}
			catch (InterruptedException e)
			{
				throw new BuildException("Waiting for instance '" + instance +
						"' to switch to '" + state + "' state was interrupted", e);
			}
		}

		if (!state.equals(getInstanceState()))
		{
			throw new BuildException("Wait time exceeded, but instance '" +
					instance + "' from '" + getMaestroProject() + "/" + getMaestroRegion() +
					"' project/region didn't switch to '" + state + "' state");
		}
	}

	@Override
	protected void validate()
	{
		if (instance == null)
		{
			throw new BuildException("Instance should be specified");
		}

		if (state == null || state.trim().isEmpty())
		{
			throw new BuildException("State should be specified");
		}
	}

	private String getInstanceState()
	{
		XMLElement element = executeCommand();

		List<XMLElement> items = element.Items();
		for (XMLElement item : items)
		{

			XMLAttribute idAttr = item.getAttribute("instanceID");
			XMLAttribute stateAttr = item.getAttribute("state");

			if (idAttr != null && idAttr.getValue() != null &&
				instance.toUpperCase().equals(idAttr.getValue().toUpperCase()))
			{
				return stateAttr.getValue();
			}
		}

		throw new BuildException("There is no instance '" + instance +
				"' in '" + getMaestroProject() + "/" + getMaestroRegion() + "' project/region");
	}
}
