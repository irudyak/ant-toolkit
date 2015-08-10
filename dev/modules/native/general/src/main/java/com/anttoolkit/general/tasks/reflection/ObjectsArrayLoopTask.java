package com.anttoolkit.general.tasks.reflection;

import java.util.*;

import org.apache.tools.ant.*;

import com.anttoolkit.general.tasks.*;

public class ObjectsArrayLoopTask
		extends GenericTask
		implements TaskContainer
{
	private String arrayRef;
	private String property;
	private String reference;
	private List<Task> tasks = new LinkedList<Task>();

	public void setArrayReference(String arrayRef)
	{
		this.arrayRef = arrayRef;
	}

	public void setProperty(String property)
	{
		this.property = property;
	}

	public void setReference(String reference)
	{
		this.reference = reference;
	}

	@Override
	public void doWork() throws BuildException
	{
		Object[] array = (Object[])this.getReference(arrayRef);

		for (Object obj : array)
		{
			if (property != null)
			{
				this.setPropertyThreadSafe(property, obj == null ? null : obj.toString());
			}

			if (reference != null)
			{
				this.setReference(reference, obj);
			}

			for (Task task : tasks)
			{
				task.perform();
			}
		}
	}

	@Override
	public void addTask(Task task)
	{
		tasks.add(task);
	}

	@Override
	protected void validate()
	{
		if (arrayRef == null || arrayRef.trim().isEmpty())
		{
			throw new BuildException("Array reference should be specified");
		}

		if (!this.checkReferenceExists(arrayRef))
		{
			throw new BuildException("Specified array reference " + arrayRef + " doesn't exist");
		}

		if (!this.getReference(arrayRef).getClass().isArray())
		{
			throw new BuildException("Specified array reference " + arrayRef + " contains not array object of type " + this.getReference(arrayRef).getClass().getName());
		}

		if (property == null && reference == null)
		{
			throw new BuildException("Property or reference name should be specified");
		}
	}
}
