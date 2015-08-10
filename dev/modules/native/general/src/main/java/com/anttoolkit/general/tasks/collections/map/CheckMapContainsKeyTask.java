package com.anttoolkit.general.tasks.collections.map;

import com.anttoolkit.general.tasks.collections.map.util.MapManager;
import com.anttoolkit.general.tasks.GenericTask;
import org.apache.tools.ant.*;

public class CheckMapContainsKeyTask
		extends GenericTask
{
	private String mapName = null;
	private String key = null;
	private String property = null;

	public void setMap(String name)
	{
		mapName = name;
	}

	public void setKey(String key)
	{
		this.key = key;
	}

	public void setProperty(String property)
	{
		this.property = property;
	}

	public void doWork() throws BuildException
	{
		boolean contains = MapManager.containsKey(mapName, key);
		setPropertyThreadSafe(property, Boolean.toString(contains));
	}

	protected void validate()
	{
		if (mapName == null)
		{
			throw new BuildException("Map name should be specified");
		}

		if (key == null)
		{
			throw new BuildException("Map key should be specified");
		}

		if (property == null)
		{
			throw new BuildException("Ant property name should be specified");
		}
	}
}
