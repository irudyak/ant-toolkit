package com.anttoolkit.general.tasks.collections.queue;

import java.util.*;

import org.apache.tools.ant.*;

import com.anttoolkit.general.common.*;
import com.anttoolkit.general.tasks.*;
import com.anttoolkit.general.tasks.collections.queue.util.*;

public class ProcessQueueTask
		extends GenericTask
		implements TaskContainer
{
	private String queueName;
	private String property;
	private int timeout = 200;
	private long wait = 0;
	private String blockingPrefix;
	private boolean echo = false;

	private List<Task> tasks = new LinkedList<Task>();

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

	public void setBlockingPrefix(String prefix)
	{
		blockingPrefix = prefix;
	}

	public void setEcho(boolean echo)
	{
		this.echo = echo;
	}

	@Override
	public void addTask(Task task)
	{
		tasks.add(task);
	}

	@Override
	public void doWork() throws BuildException
	{
		Object sync = SynchronizationManager.getSynchronizationObject("queue", queueName);

		do
		{
			echo("Waiting to take '" + queueName + "' queue ownership...");

			String message = null;

			synchronized (sync)
			{
				echo("Got '" + queueName + "' queue ownership");

				message = getNextMessage();
				if (message == null)
				{
					echo("Finished processing '" + queueName + "' queue, cause it's empty and no new messages appeared");
					return;
				}

				if (blockingPrefix != null && message.startsWith(blockingPrefix))
				{
					processMessage(message);
					message = null;
				}

				echo("Released '" + queueName + "' queue ownership");
			}

			if (message != null)
			{
				processMessage(message);
			}
		} while (true);
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

		if (blockingPrefix != null && blockingPrefix.isEmpty())
		{
			throw new BuildException("Blocking prefix can't be empty");
		}
	}

	private String getNextMessage()
	{
		String message = QueueManager.extract(queueName);
		if (message != null)
		{
			return message;
		}

		long start = System.currentTimeMillis();

		while (message == null && System.currentTimeMillis() - start <= wait)
		{
			echo("Waiting for a new message to appear in '" + queueName + "' queue...");

			try
			{
				Thread.sleep(timeout);
			}
			catch (InterruptedException e)
			{
				throw new BuildException("Waiting for queue '" + queueName + "' items was interrupted", e);
			}

			message = QueueManager.extract(queueName);
		}

		return message;
	}

	private void processMessage(String message)
	{
		this.setPropertyThreadSafe(property, message);

		boolean isBlocking = blockingPrefix != null && message.startsWith(blockingPrefix);

		if (isBlocking)
		{
			echo("Processing BLOCKING message '" + message + "' from '" + queueName + "' queue...");
		}
		else
		{
			echo("Processing NON-BLOCKING message '" + message + "' from '" + queueName + "' queue...");
		}

		for (Task task : tasks)
		{
			task.perform();
		}

		if (isBlocking)
		{
			echo("Finished processing BLOCKING message '" + message + "' from '" + queueName + "' queue");
		}
		else
		{
			echo("Finished processing NON-BLOCKING message '" + message + "' from '" + queueName + "' queue");
		}
	}

	private void echo(String msg)
	{
		if (echo)
		{
			log(msg);
		}
	}
}
