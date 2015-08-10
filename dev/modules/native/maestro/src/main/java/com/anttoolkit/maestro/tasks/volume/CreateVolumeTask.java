package com.anttoolkit.maestro.tasks.volume;

import java.util.*;

import org.apache.tools.ant.*;

import com.maestro.xml.*;

import com.anttoolkit.maestro.common.*;

@Action("create-attach-volume")
public class CreateVolumeTask extends VolumeTask
{
	@Param("instance-id")
	private String instance;

	@Param
	private Integer size;

	@Param
	private String device;

	@Param("snapshot-id")
	private String snapshot;

	private String property;

	public void setInstance(String instance)
	{
		this.instance = instance;
	}

	public void setSize(int size)
	{
		this.size = size;
	}

	public void setDevice(String device)
	{
		this.device = device;
	}

	public void setSnapshot(String snapshot)
	{
		this.snapshot = snapshot;
	}

	public void setProperty(String property)
	{
		this.property = property;
	}

	@Override
	public void doWork() throws BuildException
	{
		XMLElement element = executeCommand();

		Map<String, Map<String, String>> info = parseVolumesInfo(element);
		if (info == null || info.isEmpty())
		{
			throw new BuildException("Failed to create new volume for unknown reason");
		}

		String volumeId = info.keySet().iterator().next();

		this.printResult(element);

		if (property != null)
		{
			this.setPropertyThreadSafe(property, volumeId);
		}

		if (!isAsync())
		{
			this.waitVolumeStateToChange(volumeId, "ready");
		}
	}
}
