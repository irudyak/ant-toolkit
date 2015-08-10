package com.anttoolkit.general.tasks.collections.map;

import com.anttoolkit.general.tasks.collections.map.util.MapManager;
import org.apache.tools.ant.*;

import com.anttoolkit.general.tasks.*;

public class DestroyMapTask
		extends GenericTask
{
	private String mapName = null;

	public void setMap(String name)
	{
		mapName = name;
	}

	public void doWork() throws BuildException
	{
		MapManager.destroy(mapName);
	}

	protected void validate()
	{
		if (mapName == null)
		{
			throw new BuildException("Map name should be specified");
		}
	}
}
