package com.anttoolkit.general.tasks.collections.map;

import java.util.*;

import org.apache.tools.ant.*;

import com.anttoolkit.general.tasks.*;
import com.anttoolkit.general.common.*;
import com.anttoolkit.general.entities.*;

public class SerializeMapTask
		extends GenericTask
		implements IEntityProcessor<Map<String, String>, Void, Void>
{
	private String mapName;
	private String file;
	private String keyValueSeparator = ",";
	private String entriesSeparator = SystemHelper.lineSeparator;
	private String property;

	public void setMap(String map)
	{
		this.mapName = map;
	}

	public void setFile(String file)
	{
		this.file = file;
	}

	public void setProperty(String property)
	{
		this.property = property;
	}

	public void setKeyValueSeparator(String separator)
	{
		this.keyValueSeparator = separator;
	}

	public void setEntriesSeparator(String separator)
	{
		this.entriesSeparator = separator;
	}

	@Override
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
		StringBuilder builder = new StringBuilder();

		boolean firstIteration = true;

		Set<String> keys = entity.keySet();
		for (String key : keys)
		{
			String value = entity.get(key);

			if (!firstIteration)
			{
				builder.append(entriesSeparator);
			}

			builder.append(key).append(keyValueSeparator).append(value);

			firstIteration = false;
		}

		String value = builder.toString();

		if (property != null)
		{
			this.setPropertyThreadSafe(property, value);
		}

		if (file != null)
		{
			this.saveContentToFile(file, builder.toString());
		}

		return null;
	}

	@Override
	public boolean readOnly()
	{
		return true;
	}

	@Override
	protected void validate()
	{
		if (mapName == null || mapName.trim().length() == 0)
		{
			throw new BuildException("Map name doesn't specified");
		}

		if (entriesSeparator == null || entriesSeparator.length() == 0)
		{
			throw new BuildException("Map entries separator doesn't specified");
		}

		if (keyValueSeparator == null || keyValueSeparator.length() == 0)
		{
			throw new BuildException("Map key and value separator doesn't specified");
		}

		if (property == null && file == null)
		{
			throw new BuildException("File name and/or property should be specified");
		}

		if (file != null && this.dirExists(file))
		{
			throw new BuildException("Directory with the same name '" + file + "' like a file were you plan to serialize the map '" + mapName + "' already exists");
		}
	}
}
