package com.anttoolkit.maestro.tasks.properties;

import java.util.*;

import org.apache.tools.ant.*;

import com.anttoolkit.maestro.common.*;
import com.anttoolkit.maestro.tasks.properties.util.*;

@Action("set-properties")
public class SetPropertiesTask extends PropertyTask
{
	@Param("property-secure")
	private boolean secure = false;

	@Param
	private boolean append = false;

	@Param(value = "property-list", keyValueList = true)
	private List<String> properties = new LinkedList<String>();

	public void setSecure(boolean secure)
	{
		this.secure = secure;
	}

	public void setAppend(boolean append)
	{
		this.append = append;
	}

	public void setInstance(String instances)
	{
		this.initInstances(instances);
	}

	public void setVolume(String volumes)
	{
		this.initVolumes(volumes);
	}

	public void addConfiguredProperty(Property property)
	{
		property.validate();
		properties.add(property.getName() + "=" + property.getValue());
	}

	@Override
	public void doWork() throws BuildException
	{
		executeCommand();
	}
}
