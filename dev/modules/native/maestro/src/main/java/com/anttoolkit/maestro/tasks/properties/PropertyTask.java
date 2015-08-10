package com.anttoolkit.maestro.tasks.properties;

import java.util.*;

import org.apache.tools.ant.*;

import com.anttoolkit.maestro.common.*;
import com.anttoolkit.maestro.tasks.*;

public abstract class PropertyTask extends CliCommandTask
{
	@Param(sorted = true)
	private Set<String> instances = new HashSet<String>();

	@Param(sorted = true)
	private Set<String> volumes = new HashSet<String>();

	protected void addInstance(String instance)
	{
		instances.add(instance);
	}

	protected void addVolume(String volume)
	{
		volumes.add(volume);
	}

	protected void initInstances(String instances)
	{
		populateParamValues(instances, this.instances);
	}

	protected void initVolumes(String volumes)
	{
		populateParamValues(volumes, this.volumes);
	}

	@Override
	protected void validate()
	{
		if (instances.isEmpty() && volumes.isEmpty())
		{
			throw new BuildException("No instance or volumes specified");
		}
	}
}
