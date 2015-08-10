package com.anttoolkit.general.tasks.collections.map;

import com.anttoolkit.general.tasks.collections.map.util.MapManager;
import com.anttoolkit.general.tasks.GenericTask;
import org.apache.tools.ant.*;

public class GetMapSizeTask
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
		int size = MapManager.size(mapName);
		this.setPropertyThreadSafe(property, Integer.toString(size));
	}

	protected void validate()
	{
		if (mapName == null)
		{
			throw new BuildException("Map name should be specified");
		}

		if (property == null)
		{
			throw new BuildException("Property name should be specified");
		}
	}
}