package com.anttoolkit.general.tasks.collections.array;

import org.apache.tools.ant.*;

import java.util.*;

import com.anttoolkit.general.tasks.*;
import com.anttoolkit.general.tasks.collections.array.util.*;

public class InitArrayTask
		extends GenericTask
{
	private String name;
	private String values;
	private String file;
	private String separator = ",";
	private boolean trimValues = true;
	private boolean removeEmptyValues = false;
	private String sizeProperty;
	private boolean ignoreIfExist = false;

	public void setName(String name)
	{
		this.name = name;
	}

	public void setValues(String values)
	{
		this.values = values;
	}

	public void addText(String values)
	{
		this.values = values;
	}

	public void setFile(String file)
	{
		this.file = file;
	}

	public void setSeparator(String separator)
	{
		this.separator = separator;
	}

	public void setTrimValues(boolean trim)
	{
		trimValues = trim;
	}

	public void setRemoveEmptyValues(boolean remove)
	{
		removeEmptyValues = remove;
	}

	public void setSizeProperty(String property)
	{
		sizeProperty = property;
	}

	public void setIgnoreIfExist(boolean ignore)
	{
		ignoreIfExist = ignore;
	}

	public void doWork()
			throws BuildException
	{
		List<String> data = getArrayData();

		ArrayManager.init(name, data, ignoreIfExist);

		if (sizeProperty != null)
		{
			this.setPropertyThreadSafe(sizeProperty, Integer.toString(data.size()));
		}
	}

	protected void validate()
	{
		if (name == null || name.trim().length() == 0)
		{
			throw new BuildException("Array name doesn't specified");
		}

		if (separator == null || separator.length() == 0)
		{
			throw new BuildException("Array elements separator doesn't specified");
		}
	}

	private List<String> getArrayData()
	{
		List<String> data = new ArrayList<String>(256);

		if (values != null)
		{
			Collections.addAll(data, values.split(separator, -1));
		}
		else if (file != null)
		{
			String content = this.loadFileContent(file);
			String[] array = content != null && !content.isEmpty() ? content.split(separator, -1) : null;

			if (array != null)
			{
				Collections.addAll(data, array);
			}
		}
		else
		{
			return data;
		}

		if (!trimValues && !removeEmptyValues)
		{
			return data;
		}

		List<String> filteredData = new ArrayList<String>(256);

		for (String val : data)
		{
			if (trimValues)
			{
				val = val.trim();
			}

			if (!removeEmptyValues || val.length() != 0)
			{
				filteredData.add(val);
			}
		}

		return filteredData;
	}
}
