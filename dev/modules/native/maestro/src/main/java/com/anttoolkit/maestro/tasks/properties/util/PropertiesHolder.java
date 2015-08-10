package com.anttoolkit.maestro.tasks.properties.util;

import org.apache.tools.ant.*;

import com.anttoolkit.general.tasks.collections.map.util.*;

public class PropertiesHolder
{
	private String id;
	private String propertiesMap;

	public void setId(String id)
	{
		this.id = id;
	}

	public void setPropertiesMap(String map)
	{
		this.propertiesMap = map;
	}

	public String getId()
	{
		return id;
	}

	public String getPropertiesMap()
	{
		return propertiesMap;
	}

	public void addProperty(String name, String value)
	{
		MapManager.put(propertiesMap, name, value);
	}

	public void validate(String resourceType)
	{
		if (id == null)
		{
			throw new BuildException(resourceType + " id wasn't specified");
		}

		if (propertiesMap == null)
		{
			throw new BuildException("No propertiesMap specified for " + resourceType + ": " + id);
		}

		if (!MapManager.exists(propertiesMap))
		{
			throw new BuildException("Specified map '" + propertiesMap + "' doesn't exist");
		}
	}
}
