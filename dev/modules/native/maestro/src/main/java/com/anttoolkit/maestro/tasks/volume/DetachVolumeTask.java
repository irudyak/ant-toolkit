package com.anttoolkit.maestro.tasks.volume;

import org.apache.tools.ant.*;

import com.anttoolkit.maestro.common.*;

@Action("detach-volume")
public class DetachVolumeTask extends VolumeTask
{
	@Param("volume-id")
	private String volume;

	public void setVolume(String volume)
	{
		this.volume = volume;
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

	@Override
	protected void validate()
	{
		if (volume == null)
		{
			throw new BuildException("Volume should be specified");
		}
	}
}
