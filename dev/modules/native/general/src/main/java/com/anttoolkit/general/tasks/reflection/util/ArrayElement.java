package com.anttoolkit.general.tasks.reflection.util;

import org.apache.tools.ant.*;

import com.anttoolkit.general.common.*;
import com.anttoolkit.general.tasks.*;

public class ArrayElement
{
	private String value;
	private String reference;
	private Integer index;

	public void setValue(String value)
	{
		this.value = value;
	}

	public void setReference(String reference)
	{
		this.reference = reference;
	}

	public void setIndex(int index)
	{
		this.index = index;
	}

	public Object getValue(String clazz, GenericTask task)
	{
		validate();

		if (reference != null)
		{
			Object obj = task.getReference(reference);
			if (obj == null)
			{
				return null;
			}

			try
			{
				if (!ReflectionHelper.extendsClass(obj.getClass(), ReflectionHelper.forName(clazz)))
				{
					throw new BuildException("Reference '" + reference + "' doesn't hold object of type " + clazz);
				}

				return obj;
			}
			catch (ClassNotFoundException e)
			{
				throw new BuildException("Failed to initialize array element of class " + clazz, e);
			}
		}

		if (String.class.getName().equals(clazz))
		{
			return value;
		}

		try
		{
			if (!ReflectionHelper.hasConstructor(clazz, new Class[] {String.class}))
			{
				throw new BuildException("Don't know how to convert string value '" + value + "' " +
						"to an instance of object " + clazz);
			}

			return ReflectionHelper.newInstance(clazz, new Object[] {value}, new Class[] {String.class});
		}
		catch (ClassNotFoundException e)
		{
			throw new BuildException("Failed to initialize array element of class " + clazz, e);
		}
		catch (Exception e)
		{
			throw new BuildException("Failed to convert string value '" + value + "' " +
					"to an instance of object " + clazz);
		}
	}

	public int getIndex()
	{
		return index == null ? -1 : index;
	}

	public void validate()
	{
		if ((value == null && reference == null) ||
			(value != null && reference != null))
		{
			throw new BuildException("Either value or reference should be specified");
		}

		if (index != null && index < 0)
		{
			throw new BuildException("Incorrect array element index " + index + " specified");
		}
	}
}
