package com.anttoolkit.maestro.tasks.properties;

import java.util.*;

import org.apache.tools.ant.*;

import com.anttoolkit.maestro.common.*;

@Action("delete-instance-properties")
public class DeletePropertiesTask extends PropertyTask
{
	@Param
	private boolean all = false;

	@Param("property-name-list")
	private Set<String> properties = new HashSet();

	public void setAll(boolean all)
	{
		this.all = all;
	}

	public void setInstance(String instances)
	{
		this.initInstances(instances);
	}

	public void setVolume(String volumes)
	{
		this.initVolumes(volumes);
	}

	public void setProperties(String properties)
	{
		populateParamValues(properties, this.properties);
	}

	@Override
	public void doWork() throws BuildException
	{
		executeCommand();
	}
}
