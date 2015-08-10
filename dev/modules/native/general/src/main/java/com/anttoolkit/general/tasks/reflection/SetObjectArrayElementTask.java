package com.anttoolkit.general.tasks.reflection;

import java.lang.reflect.*;
import java.util.*;

import org.apache.tools.ant.*;

import com.anttoolkit.general.tasks.*;
import com.anttoolkit.general.tasks.reflection.util.*;

public class SetObjectArrayElementTask extends GenericTask
{
	private String arrayRef;
	private List<ArrayElement> elements = new LinkedList<ArrayElement>();

	public void setArrayReference(String arrayRef)
	{
		this.arrayRef = arrayRef;
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
		Object arrayObj = this.getReference(arrayRef);
		String elementClass = arrayObj.getClass().getComponentType().toString();

		for (int i = 0; i < elements.size(); i++)
		{
			ArrayElement element = elements.get(i);
			int index = element.getIndex() != -1 ? element.getIndex() : i;
			Array.set(arrayObj, index, element.getValue(elementClass, this));
		}
	}

	protected void validate()
	{
		if (arrayRef == null || arrayRef.trim().isEmpty())
		{
			throw new BuildException("Array reference should be specified");
		}

		if (elements.isEmpty())
		{
			throw new BuildException("No array elements specified");
		}

		if (this.checkReferenceExists(arrayRef))
		{
			throw new BuildException("Array reference " + arrayRef + " doesn't exist");
		}

		Class clazz = this.getReference(arrayRef).getClass();
		if (!clazz.isArray())
		{
			throw new BuildException("Array reference " + arrayRef + " contains reference to non array object " + clazz.getName());
		}
	}
}
