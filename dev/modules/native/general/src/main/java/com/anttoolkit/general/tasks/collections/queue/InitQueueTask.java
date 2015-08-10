package com.anttoolkit.general.tasks.collections.queue;

import java.util.*;

import org.apache.tools.ant.*;

import com.anttoolkit.general.tasks.*;
import com.anttoolkit.general.tasks.collections.queue.util.*;

public class InitQueueTask extends GenericTask
{
	private String name;
	private String values;
	private String file;
	private String separator = ",";
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
		List<String> data = getQueueData();

		QueueManager.init(name, data, ignoreIfExist);

		if (sizeProperty != null)
		{
			this.setPropertyThreadSafe(sizeProperty, Integer.toString(data.size()));
		}
	}

	protected void validate()
	{
		if (name == null || name.trim().length() == 0)
		{
			throw new BuildException("Queue name doesn't specified");
		}

		if (separator == null || separator.length() == 0)
		{
			throw new BuildException("Queue elements separator doesn't specified");
		}
	}

	private List<String> getQueueData()
	{
		List<String> data = new LinkedList<String>();

		if (values != null && !values.isEmpty())
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

		for (String val : data)
		{
			if (val.isEmpty())
			{
				throw new BuildException("Queue can't contain empty messages");
			}
		}

		return data;
	}
}
