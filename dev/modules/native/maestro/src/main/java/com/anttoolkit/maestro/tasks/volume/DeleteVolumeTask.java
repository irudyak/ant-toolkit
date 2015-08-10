package com.anttoolkit.maestro.tasks.volume;

import org.apache.tools.ant.*;

import com.anttoolkit.maestro.common.*;

@Action("delete-volume")
public class DeleteVolumeTask extends VolumeTask
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
	}
}
