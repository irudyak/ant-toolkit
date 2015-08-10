package com.anttoolkit.maestro.tasks.volume;

import org.apache.tools.ant.*;

import com.anttoolkit.maestro.common.*;

@Action("attach-volume")
public class AttachVolumeTask extends VolumeTask
{
	@Param("volume-id")
	private String volume;

	@Param("instance-id")
	private String instance;

	@Param
	private String device;

	public void setVolume(String volume)
	{
		this.volume = volume;
	}

	public void setInstance(String instance)
	{
		this.instance = instance;
	}

	public void setDevice(String device)
	{
		this.device = device;
	}

	@Override
	public void doWork() throws BuildException
	{
		executeCommand();

		if (!isAsync())
		{
			this.waitVolumeStateToChange(volume, "ready");
		}
	}
}
