package com.anttoolkit.general.tasks.collections.map;

import com.anttoolkit.general.tasks.collections.map.util.MapManager;
import com.anttoolkit.general.tasks.GenericTask;
import org.apache.tools.ant.*;

public class GetMapElementTask
		extends GenericTask
{
	private String mapName;
	private String key;
	private String property;
	private String defaultValue = "";

	public void setMap(String name)
	{
		mapName = name;
	}

	public void setKey(String key)
	{
		this.key = key;
	}

	public void setProperty(String name)
	{
		property = name;
	}

	public void setDefault(String value)
	{
		this.defaultValue = value;
	}

	public void doWork()
			throws BuildException
	{
		String value = MapManager.get(mapName, key);
		this.setPropertyThreadSafe(property, value == null ? defaultValue : value);
	}

	protected void validate()
	{
		if (mapName == null || mapName.trim().length() == 0)
		{
			throw new BuildException("Map name doesn't specified");
		}

		if (key == null)
		{
			throw new BuildException("Mapy key doesn't specified");
		}

		if (property == null || property.trim().length() == 0)
		{
			throw new BuildException("Property name doesn't specified");
		}
	}
}
