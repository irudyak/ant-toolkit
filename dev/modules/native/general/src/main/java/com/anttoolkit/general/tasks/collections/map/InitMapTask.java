package com.anttoolkit.general.tasks.collections.map;

import java.io.*;
import java.util.*;

import org.apache.tools.ant.*;

import com.anttoolkit.general.common.*;
import com.anttoolkit.general.tasks.collections.map.util.*;
import com.anttoolkit.general.tasks.*;

public class InitMapTask
		extends GenericTask
{
	private String name;
	private String file;
	private String values;
	private Map<String, String> configuredMap = new HashMap<String, String>();
	private String keyValueSeparator = ",";
	private String entriesSeparator = SystemHelper.lineSeparator;
	private boolean trimValues = true;
	private String sizeProperty;
	private boolean ignoreIfExist = false;

	public static class MapEntry
	{
		private String key = null;
		private String value = null;

		public void setKey(String name)
		{
			key = name;
		}

		public String getKey()
		{
			return key;
		}

		public void setValue(String value)
		{
			this.value = value;
		}

		public String getValue()
		{
			return value;
		}
	}

	public void setName(String name)
	{
		this.name = name;
	}

	public void addConfiguredEntry(MapEntry entry)
	{
		configuredMap.put(entry.getKey(), entry.getValue());
	}

	public void setFile(String file)
	{
		this.file = file;
	}

	public void setValues(String values)
	{
		this.values = values;
	}

	public void setIgnoreIfExist(boolean ignore)
	{
		ignoreIfExist = ignore;
	}

	public void setKeyValueSeparator(String separator)
	{
		this.keyValueSeparator = separator;
	}

	public void setEntriesSeparator(String separator)
	{
		this.entriesSeparator = separator;
	}

	public void setTrimValues(boolean trim)
	{
		trimValues = trim;
	}

	public void setSizeProperty(String property)
	{
		sizeProperty = property;
	}

	public void doWork()
			throws BuildException
	{
		Map<String, String> data = getMapData();

		MapManager.init(name, data, ignoreIfExist);

		if (sizeProperty != null)
		{
			this.setPropertyThreadSafe(sizeProperty, Integer.toString(data.size()));
		}
	}

	@Override
	protected void validate()
	{
		if (name == null || name.trim().length() == 0)
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
	}

	private Map<String, String> getMapData()
	{
		if (file == null && values == null)
		{
			return configuredMap;
		}

		if (values != null)
		{
			addToConfiguredMap(values);
		}

		if (file != null)
		{
			String fullPath = getFileFullPath(file);

			File fileObj = new File(fullPath);
			if (!fileObj.isFile() || !fileObj.exists())
			{
				throw new BuildException("Incorrect file specified: " + fullPath);
			}

			String content = loadFileContent(file);
			if (content != null && !content.trim().isEmpty())
			{
				addToConfiguredMap(content);
			}
		}

		return configuredMap;
	}

	private void addToConfiguredMap(String text)
	{
		String[] entries = text.split(entriesSeparator, -1);
		for (String entry : entries)
		{
			String[] parts = entry.split(keyValueSeparator, -1);
			if (parts.length != 2)
			{
				continue;
			}

			String key = trimValues ? parts[0].trim() : parts[0];
			String value = trimValues ? parts[1].trim() : parts[1];

			configuredMap.put(key, value);
		}
	}
}
