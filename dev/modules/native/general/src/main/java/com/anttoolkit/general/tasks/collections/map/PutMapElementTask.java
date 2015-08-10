package com.anttoolkit.general.tasks.collections.map;

import com.anttoolkit.general.tasks.collections.map.util.MapManager;
import com.anttoolkit.general.tasks.GenericTask;
import org.apache.tools.ant.*;

public class PutMapElementTask
		extends GenericTask
{
	private String map;
	private String key;
	private String value;

	public void setMap(String name)
	{
		map = name;
	}

	public void setKey(String key)
	{
		this.key = key;
	}

	public void setValue(String value)
	{
		this.value = value;
	}

	public void doWork()
			throws BuildException
	{
		MapManager.put(map, key, value);
	}
}
