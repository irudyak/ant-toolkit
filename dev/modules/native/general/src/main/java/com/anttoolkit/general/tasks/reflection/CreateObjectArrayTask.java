package com.anttoolkit.general.tasks.reflection;

import java.util.*;

import org.apache.tools.ant.*;

import com.anttoolkit.general.tasks.*;
import com.anttoolkit.general.tasks.reflection.util.*;
import com.anttoolkit.general.common.*;

public class CreateObjectArrayTask extends GenericTask
{
	private String reference;
	private String className;
	private List<ArrayElement> elements = new LinkedList<ArrayElement>();

	public void setReference(String reference)
	{
		this.reference = reference;
	}

	public void setClass(String className)
	{
		this.className = className;
	}

	public void addConfiguredElement(ArrayElement element)
	{
		element.validate();
		elements.add(element);
	}

	public void addConfiguredNullElement(NullArrayElement element)
		{
			element.validate();
			elements.add(element);
	}

	@Override
	public void doWork() throws BuildException
	{
		try
		{
			Object array = ReflectionHelper.newArrayInstance(className, getValues());
			this.setReference(reference, array);
		}
		catch (Exception e)
		{
			throw new BuildException("Failed to create instance of an array for the reference '" + reference + "'", e);
		}
	}

	protected void validate()
	{
		if (className == null || className.trim().isEmpty())
		{
			throw new BuildException("Array elements class should be specified");
		}

		if (reference == null || reference.trim().isEmpty())
		{
			throw new BuildException("Reference to store created array should be specified");
		}
	}

	protected Object[] getValues()
	{
		Object[] values = new Object[elements.size()];

		for (int i = 0; i < elements.size(); i++)
		{
			values[i] = elements.get(i).getValue(className, this);
		}

		return values;
	}
}
