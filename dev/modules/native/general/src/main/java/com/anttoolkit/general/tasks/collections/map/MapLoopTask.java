package com.anttoolkit.general.tasks.collections.map;

import java.util.*;

import org.apache.tools.ant.*;

import com.anttoolkit.general.entities.*;
import com.anttoolkit.general.entities.EntityManager.*;
import com.anttoolkit.general.tasks.*;

public class MapLoopTask
		extends GenericTask
		implements TaskContainer, IEntityProcessor<Map<String, String>, Void, Map<String, String>>
{
	private List<Task> tasks = new LinkedList<Task>();

	private String map;
	private String valueProperty;
	private String keyProperty;
	private boolean useLocalCopy = true;

	public void addTask(Task task)
	{
		tasks.add(task);
	}

	public void setMap(String name)
	{
		map = name;
	}

	public void setValueProperty(String name)
	{
		valueProperty = name;
	}

	public void setKeyProperty(String name)
	{
		keyProperty = name;
	}

	public void setUseLocalCopy(boolean useCopy)
	{
		useLocalCopy = useCopy;
	}

	public void doWork()
			throws BuildException
	{
		if (map == null)
		{
			throw new BuildException("Map name should be specified");
		}

		Map<String, String> localCopy;

		try
		{
			localCopy = (Map<String, String>)EntityManager.processEntity(MapEntityType.instance, map, this, null);
		}
		catch (EntityNotFoundException e)
		{
			throw new BuildException("There was no map " + map + " previously initialized", e);
		}

		if (useLocalCopy && localCopy != null)
		{
			processMap(localCopy);
			localCopy.clear();
		}
	}

	@Override
	public Map<String, String> processEntity(Map<String, String> entity, Void param)
	{
		if (!useLocalCopy)
		{
			processMap(entity);
			return null;
		}

		Map<String, String> localCopy = new HashMap<String, String>();

		Set<String> keys = entity.keySet();
		for (String key : keys)
		{
			localCopy.put(key, entity.get(key));
		}

		return localCopy;
	}

	@Override
	public boolean readOnly()
	{
		return true;
	}

	private void processMap(Map<String, String> data)
	{
		Set<String> keys = data.keySet();
		for (String key : keys)
		{
			if (keyProperty != null)
			{
				this.setPropertyThreadSafe(keyProperty, key);
			}

			if (valueProperty != null)
			{
				this.setPropertyThreadSafe(valueProperty, data.get(key));
			}

			for (Task task : tasks)
			{
				task.perform();
			}
		}
	}
}
