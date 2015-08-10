package com.anttoolkit.general.tasks.collections.map;

import org.apache.tools.ant.*;

import com.anttoolkit.general.tasks.*;
import com.anttoolkit.general.tasks.collections.map.util.*;

public class CheckMapExistsTask
		extends GenericTask
{
	private String mapName = null;
	private String property = null;

	public void setMap(String name)
	{
		mapName = name;
	}

	public void setProperty(String property)
	{
		this.property = property;
	}

	public void doWork() throws BuildException
	{
		setPropertyThreadSafe(property, Boolean.toString(MapManager.exists(mapName)));
	}

	protected void validate()
	{
		if (mapName == null)
		{
			throw new BuildException("Map name should be specified");
		}

		if (property == null)
		{
			throw new BuildException("Ant property name should be specified");
		}
	}
}
