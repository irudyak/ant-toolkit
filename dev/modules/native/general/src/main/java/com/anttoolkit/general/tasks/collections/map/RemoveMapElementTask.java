package com.anttoolkit.general.tasks.collections.map;

import org.apache.tools.ant.*;

import com.anttoolkit.general.tasks.*;
import com.anttoolkit.general.tasks.collections.map.util.*;

public class RemoveMapElementTask
		extends GenericTask
{
	private String mapName;
	private String key;

	public void setMap(String name)
	{
		mapName = name;
	}

	public void setKey(String key)
	{
		this.key = key;
	}

	public void doWork()
			throws BuildException
	{
		MapManager.remove(mapName, key);
	}

	protected void validate()
	{
		if (mapName == null || mapName.trim().length() == 0)
		{
			throw new BuildException("Map name doesn't specified");
		}

		if (key == null)
		{
			throw new BuildException("Map key doesn't specified");
		}
	}
}
