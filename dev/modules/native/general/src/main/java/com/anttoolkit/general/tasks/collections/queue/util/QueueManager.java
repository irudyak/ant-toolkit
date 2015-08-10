package com.anttoolkit.general.tasks.collections.queue.util;

import java.util.*;

import org.apache.tools.ant.*;

import com.anttoolkit.general.entities.*;

public class QueueManager
{
	private static final IEntityProcessor CLEANER = new Cleaner();
	private static final IEntityProcessor EXTRACTOR = new Extractor();
	private static final IEntityProcessor ADDER = new Adder();
	private static final IEntityProcessor SIZER = new Sizer();
	private static final IEntityProcessor COPIER = new Copier();

	public static void clear(String queueName, boolean silentIfNotExists)
	{
		try
		{
			EntityManager.processEntity(QueueEntityType.instance, globalName(queueName), CLEANER, null);
		}
		catch (EntityNotFoundException e)
		{
			if (!silentIfNotExists)
			{
				throw new BuildException("There is no queue '" + queueName + "' previously initialized", e);
			}
		}
	}

	public static String extract(String queueName)
	{
		try
		{
			return (String)EntityManager.processEntity(QueueEntityType.instance, globalName(queueName), EXTRACTOR, null);
		}
		catch (EntityNotFoundException e)
		{
			throw new BuildException("There was no queue " + queueName + " previously initialized", e);
		}
	}

	public static void add(String queueName, String value)
	{
		if (value == null || value.isEmpty())
		{
			throw new BuildException("It is not allowed to put empty string into queue");
		}

		try
		{
			EntityManager.processEntity(QueueEntityType.instance, globalName(queueName), ADDER, value);
		}
		catch (EntityNotFoundException e)
		{
			throw new BuildException("There was no queue " + queueName + " previously initialized", e);
		}
	}

	public static int size(String queueName)
	{
		try
		{
			return (Integer)EntityManager.processEntity(QueueEntityType.instance, globalName(queueName), SIZER, null);
		}
		catch (EntityNotFoundException e)
		{
			throw new BuildException("There was no queue " + queueName + " previously initialized", e);
		}
	}

	public static void init(String queueName, List<String> data, boolean ignoreIfAlreadyExist)
	{
		try
		{
			EntityManager.setEntity(QueueEntityType.instance, globalName(queueName), data, ignoreIfAlreadyExist);
		}
		catch (EntityStorageAbsentException e)
		{
			throw new BuildException("There are no storage for queue data", e);
		}
	}

	public static void destroy(String queueName)
	{
		EntityManager.removeEntity(QueueEntityType.instance, globalName(queueName));
	}

	public static boolean exists(String queueName)
	{
		return EntityManager.exists(QueueEntityType.instance, globalName(queueName));
	}

	public static void copy(String src, String dest)
	{
		try
		{
			EntityManager.processEntity(QueueEntityType.instance, globalName(src), COPIER, dest);
		}
		catch (EntityNotFoundException e)
		{
			throw new BuildException("There was no queue " + src + " previously initialized", e);
		}
	}

	public static String globalName(String queueName)
	{
		return EntityManager.GLOBAL_SCOPE_PREFIX + queueName;
	}
}
