package com.anttoolkit.maestro.tasks.volume;

import java.util.*;

import com.anttoolkit.maestro.tasks.utils.*;
import com.anttoolkit.maestro.common.*;

@Action("describe-volumes")
public class VolumesLoopTask extends XmlItemLoopTask
{
	@Param("instance-id")
	private String instance;

	@Param(value = "volumes", sorted = true)
	private Set<String> volumes = new HashSet<String>();

	public void setInstance(String instance)
	{
		this.instance = instance;
	}

	public void setVolumes(String volumes)
	{
		this.populateParamValues(volumes, this.volumes);
	}
}
