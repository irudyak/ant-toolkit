package com.anttoolkit.general.tasks.collections.queue;

import org.apache.tools.ant.*;

import com.anttoolkit.general.tasks.*;
import com.anttoolkit.general.tasks.collections.queue.util.*;

public class DestroyQueueTask
		extends GenericTask
{
	private String queueName = null;

	public void setQueue(String name)
	{
		queueName = name;
	}

	public void doWork() throws BuildException
	{
		if (queueName == null)
		{
			throw new BuildException("Queue name should be specified");
		}

		QueueManager.destroy(queueName);
	}
}
