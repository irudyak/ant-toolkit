package com.anttoolkit.maestro.tasks.volume;

import org.apache.tools.ant.*;

import com.anttoolkit.maestro.common.*;

@Action("resize-volume")
public class ResizeVolumeTask extends VolumeTask
{
	@Param("volume-id")
	private String volume;

	@Param
	private Integer size;

	public void setVolume(String volume)
	{
		this.volume = volume;
	}

	public void setSize(int size)
	{
		this.size= size;
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
