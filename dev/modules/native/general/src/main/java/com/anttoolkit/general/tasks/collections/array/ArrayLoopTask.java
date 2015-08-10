package com.anttoolkit.general.tasks.collections.array;

import java.util.*;

import org.apache.tools.ant.*;

import com.anttoolkit.general.tasks.*;
import com.anttoolkit.general.entities.*;

public class ArrayLoopTask
		extends GenericTask
		implements TaskContainer, IEntityProcessor<List<String>, Void, List<String>>
{
	private static final String FORWARD_DIRECTION = "forward";
	private static final String BACK_DIRECTION = "back";

	private List<Task> tasks = new LinkedList<Task>();

	private String array;
	private String elementProperty;
	private String indexProperty;
	private String direction = FORWARD_DIRECTION;
	private boolean useLocalCopy = true;

	public void addTask(Task task)
	{
		tasks.add(task);
	}

	public void setArray(String name)
	{
		array = name;
	}

	public void setElementProperty(String name)
	{
		elementProperty = name;
	}

	public void setIndexProperty(String name)
	{
		indexProperty = name;
	}

	public void setDirection(String direction)
	{
		if (!FORWARD_DIRECTION.equals(direction.trim().toLowerCase()) &&
			!BACK_DIRECTION.equals(direction.trim().toLowerCase()))
		{
			throw new BuildException("Invalid direction specified: " + direction);
		}

		this.direction = direction.trim().toLowerCase();
	}

	public void setUseLocalCopy(boolean useCopy)
	{
		useLocalCopy = useCopy;
	}

	public void doWork()
			throws BuildException
	{
		int tasksCount = tasks.size();
		if (tasksCount == 0)
		{
			return;
		}

		List<String> localCopy;

		try
		{
			localCopy = (List<String>)EntityManager.processEntity(ArrayEntityType.instance, array, this, null);
		}
		catch (EntityNotFoundException e)
		{
			throw new BuildException("Array " + array + " wasn't previously initialized", e);
		}

		if (useLocalCopy && localCopy != null)
		{
			processArray(localCopy);
			localCopy.clear();
		}
	}

	@Override
	public List<String> processEntity(List<String> data, Void param)
	{
		if (!useLocalCopy)
		{
			processArray(data);
			return null;
		}

		List<String> localCopy = new ArrayList<String>(data.size());
		for (String value : data)
		{
			localCopy.add(value);
		}

		return localCopy;
	}

	@Override
	public boolean readOnly()
	{
		return true;
	}

	private void processArray(List<String> data)
	{
		int count = data.size();

		int inc = FORWARD_DIRECTION.equals(direction) ? 1 : -1;
		int start = inc > 0 ? 0 : count - 1;
		int end = inc > 0 ? count - 1 : 0;

		for (int i = start; inc > 0 ? i <= end : i >= end; i = i + inc)
		{
			if (indexProperty != null)
			{
				this.setPropertyThreadSafe(indexProperty, Integer.toString(i));
			}

			if (elementProperty != null)
			{
				this.setPropertyThreadSafe(elementProperty, data.get(i));
			}

			for (Task task : tasks)
			{
				task.perform();
			}
		}
	}
}
