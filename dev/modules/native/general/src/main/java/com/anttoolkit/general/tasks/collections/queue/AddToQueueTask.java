package com.anttoolkit.general.tasks.collections.queue;

import org.apache.tools.ant.*;

import com.anttoolkit.general.tasks.*;
import com.anttoolkit.general.tasks.collections.queue.util.*;

public class AddToQueueTask
		extends GenericTask
{
	private String queueName;
	private String value = null;

	public void setQueue(String name)
	{
		queueName = name;
	}

	public void setValue(String value)
	{
		this.value = value;
	}

	public void addText(String value)
	{
		this.value = value;
	}

	public void doWork()
			throws BuildException
	{
		QueueManager.add(queueName, value);
	}
}
