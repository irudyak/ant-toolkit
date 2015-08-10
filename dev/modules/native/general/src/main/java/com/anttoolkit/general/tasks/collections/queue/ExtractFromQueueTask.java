package com.anttoolkit.general.tasks.collections.queue;

import org.apache.tools.ant.*;

import com.anttoolkit.general.tasks.*;
import com.anttoolkit.general.tasks.collections.queue.util.*;

public class ExtractFromQueueTask
		extends GenericTask
{
	private String queueName;
	private String property;
	private int timeout = 200;
	private long wait = 0;

	public void setQueue(String name)
	{
		queueName = name;
	}

	public void setTimeout(int timeout)
	{
		if (timeout <= 0)
		{
			throw new BuildException("Timeout should be greater than zero");
		}

		this.timeout = timeout;
	}

	public void setWait(long wait)
	{
		if (wait < 0)
		{
			throw new BuildException("Wait time should be greater or equal to zero");
		}

		this.wait = wait;
	}

	public void setProperty(String name)
	{
		property = name;
	}

	public void doWork()
			throws BuildException
	{
		String value = QueueManager.extract(queueName);

		if (value != null || wait == 0)
		{
			this.setPropertyThreadSafe(property, value == null ? "" : value);
			return;
		}

		long start = System.currentTimeMillis();

		while (value == null && System.currentTimeMillis() - start <= wait)
		{
			try
			{
				Thread.sleep(timeout);
			}
			catch (InterruptedException e)
			{
				throw new BuildException("Waiting for queue '" + queueName + "' items was interrupted", e);
			}

			value = QueueManager.extract(queueName);
		}

		this.setPropertyThreadSafe(property, value == null ? "" : value);
	}

	protected void validate()
	{
		if (queueName == null || queueName.trim().length() == 0)
		{
			throw new BuildException("Queue name doesn't specified");
		}

		if (property == null || property.trim().length() == 0)
		{
			throw new BuildException("Property name doesn't specified");
		}
	}
}
