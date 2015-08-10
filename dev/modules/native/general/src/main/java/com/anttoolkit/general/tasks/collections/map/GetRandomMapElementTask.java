package com.anttoolkit.general.tasks.collections.map;

import java.util.*;

import com.anttoolkit.general.tasks.math.GenerateRandomIntegerTask;
import org.apache.tools.ant.*;

import com.anttoolkit.general.tasks.*;
import com.anttoolkit.general.entities.*;
import com.anttoolkit.general.tasks.collections.map.util.*;

public class GetRandomMapElementTask
		extends GenericTask
		implements IEntityProcessor<Map<String, String>, Void, Void>
{
	private String mapName = null;
	private String keyProperty = null;
	private String valueProperty = null;

	public void setMap(String name)
	{
		mapName = name;
	}

	public void setValueProperty(String name)
	{
		valueProperty = name;
	}

	public void setKeyProperty(String name)
	{
		keyProperty = name;
	}

	public void doWork() throws BuildException
	{
		try
		{
			EntityManager.processEntity(MapEntityType.instance, mapName, this, null);
		}
		catch (EntityNotFoundException e)
		{
			throw new BuildException("There was no map " + mapName + " previously initialized", e);
		}
	}

	@Override
	public Void processEntity(Map<String, String> entity, Void param)
	{
		int size = MapManager.size(mapName);
		int randomIndex = GenerateRandomIntegerTask.getRandomInt(0, size - 1);
		Set<Map.Entry<String, String>> set = entity.entrySet();

		int i = 0;
		for (Map.Entry<String, String> entry : set)
		{
			if (i == randomIndex)
			{
				if (keyProperty != null)
				{
					setPropertyThreadSafe(keyProperty, entry.getKey());
				}

				if (valueProperty != null)
				{
					setPropertyThreadSafe(valueProperty, entry.getValue());
				}

				return null;
			}

			i++;
		}

		return null;
	}

	@Override
	public boolean readOnly()
	{
		return true;
	}

	protected void validate()
	{
		if (mapName == null)
		{
			throw new BuildException("Map name should be specified");
		}

		if (valueProperty == null && keyProperty == null)
		{
			throw new BuildException("Value and key property should be specified");
		}
	}
}
