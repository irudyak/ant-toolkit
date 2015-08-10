package com.anttoolkit.maestro.tasks.volume;

import java.util.*;

import org.apache.tools.ant.*;

import com.maestro.xml.*;

import com.anttoolkit.maestro.common.*;

@Action("describe-volumes")
public class WaitVolumeStateTask extends VolumeTask
{
	public static final long WAIT_TIME = 1200000;	//20 mins
	public static final long SLEEP_TIMEOUT = 60000;	//1min

	@Param("volumes")
	String volume;

	private String state;
	private long wait = WAIT_TIME;

	public void setVolume(String volume)
	{
		this.volume = volume;
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
		if (state.equals(getVolumeState()))
		{
			return;
		}

		long start = System.currentTimeMillis();

		while (System.currentTimeMillis() - start < wait)
		{
			try
			{
				if (state.equals(getVolumeState()))
				{
					return;
				}

				Thread.sleep(SLEEP_TIMEOUT);
			}
			catch (InterruptedException e)
			{
				throw new BuildException("Waiting for volume '" + volume +
						"' to switch to '" + state + "' state was interrupted", e);
			}
		}

		if (!state.equals(getVolumeState()))
		{
			throw new BuildException("Wait time exceeded, but volume '" +
					volume + "' from '" + getMaestroProject() + "/" + getMaestroRegion() +
					"' project/region didn't switch to '" + state + "' state");
		}
	}

	@Override
	protected  void validate()
	{
		if (volume == null)
		{
			throw new BuildException("Volume should be specified");
		}
	}

	private String getVolumeState()
	{
		XMLElement element = executeCommand();

		List<XMLElement> items = element.Items();
		for (XMLElement item : items)
		{

			XMLAttribute idAttr = item.getAttribute("id");
			XMLAttribute stateAttr = item.getAttribute("state");

			if (idAttr != null && idAttr.getValue() != null &&
				volume.toUpperCase().equals(idAttr.getValue().toUpperCase()))
			{
				return stateAttr.getValue();
			}
		}

		throw new BuildException("There is no volume '" + volume +
				"' in '" + getMaestroProject() + "/" + getMaestroRegion() + "' project/region");
	}
}
