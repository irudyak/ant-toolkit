package com.anttoolkit.general.tasks.collections.queue;

import java.util.*;

import com.anttoolkit.general.tasks.collections.queue.util.QueueManager;
import org.apache.tools.ant.*;

import com.anttoolkit.general.entities.*;
import com.anttoolkit.general.tasks.*;

public class SerializeQueueTask
		extends GenericTask
		implements IEntityProcessor<List<String>, Void, Void>
{
	private String queueName = null;
	private String file = null;
	private String separator = ",";
	private String property;

	public void setQueue(String name)
	{
		queueName = name;
	}

	public void setFile(String file)
	{
		this.file = file;
	}

	public void setProperty(String property)
	{
		this.property = property;
	}

	public void setSeparator(String separator)
	{
		this.separator = separator;
	}

	@Override
	public void doWork() throws BuildException
	{
		try
		{
			EntityManager.processEntity(QueueEntityType.instance, QueueManager.globalName(queueName), this, null);
		}
		catch (EntityNotFoundException e)
		{
			throw new BuildException("Queue " + queueName + " wasn't previously initialized", e);
		}
	}

	@Override
	public Void processEntity(List<String> data, Void param)
	{
		StringBuilder builder = new StringBuilder();

		boolean firstIteration = true;

		for (String item : data)
		{
			if (!firstIteration)
			{
				builder.append(separator);
			}

			builder.append(item);

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
		if (queueName == null)
		{
			throw new BuildException("Queue name should be specified");
		}

		if (separator == null || separator.isEmpty())
		{
			throw new BuildException("Queue elements separator can't be empty");
		}

		if (property == null && file == null)
		{
			throw new BuildException("File name and/or property should be specified");
		}

		if (file != null && this.dirExists(file))
		{
			throw new BuildException("Directory with the same name '" + file + "' like a file were you plan to serialize the queue '" + queueName + "' already exists");
		}
	}
}
