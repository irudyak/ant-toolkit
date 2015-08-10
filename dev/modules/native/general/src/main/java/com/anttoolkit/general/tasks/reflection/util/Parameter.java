package com.anttoolkit.general.tasks.reflection.util;

import org.apache.tools.ant.*;

import com.anttoolkit.general.common.*;
import com.anttoolkit.general.tasks.*;

public class Parameter
{
	private String value;
	private String reference;
	private Class clazz;

	public void setValue(String value)
	{
		this.value = value;
	}

	public void setReference(String ref)
	{
		this.reference = ref;
	}

	public void setClass(String clazz)
	{
		if (clazz == null || clazz.trim().isEmpty())
		{
			throw new BuildException("Class name can't be empty");
		}

		try
		{

			this.clazz = ReflectionHelper.forName(clazz);
		}
		catch (ClassNotFoundException e)
		{
			throw new BuildException("Incorrect parameter class specified: " + clazz, e);
		}
	}

	public Object getParamValue(GenericTask task)
	{
		validate(task);

		if (reference != null)
		{
			return task.getReference(reference);
		}

		Class clazz = getParamClass(task);

		if (value == null)
		{
			if (Task.class.equals(clazz))
			{
				return task;
			}

			if (Target.class.equals(clazz))
			{
				return task.getOwningTarget();
			}

			if (Project.class.equals(clazz))
			{
				return task.getProject();
			}
		}

		if (String.class.equals(clazz))
		{
			return value;
		}

		if (!clazz.isArray())
		{
			try
			{
				return ReflectionHelper.newInstance(clazz.getName(), new Object[] {value}, new Class[] {String.class});
			}
			catch (Throwable e)
			{
				throw new BuildException("Failed to create instance of " + clazz.getName() + " class from String " + value);
			}
		}

		try
		{
			return ReflectionHelper.newArrayInstance(clazz.getName(), value.split(",", -1));
		}
		catch (Throwable e)
		{
			throw new BuildException("Failed to create instance of " + clazz.getName() + " array from String " + value);
		}
	}

	public Class getParamClass(GenericTask task)
	{
		validate(task);

		if (clazz != null)
		{
			return clazz;
		}

		if (value == null && reference == null)
		{
			return null;
		}

		if (value != null)
		{
			return clazz = String.class;
		}

		Object obj = task.getReference(reference);
		if (obj == null)
		{
			throw new BuildException("Can't determine class of the parameter referenced by '" + reference +
					"' cause referenced object is null");
		}

		return clazz = obj.getClass();
	}

	public void validate(GenericTask task)
	{
		if (value != null && reference != null)
		{
			throw new BuildException("Either value or reference should be specified");
		}

		if ((value == null && reference == null))
		{
			if (clazz != null && (Task.class.equals(clazz) || Target.class.equals(clazz) || Project.class.equals(clazz)))
			{
				return;
			}

			throw new BuildException("Value or reference should be specified");
		}

		if (reference != null)
		{
			if (!task.checkReferenceExists(reference))
			{
				throw new BuildException("Specified reference '" + reference + "' doesn't exist");
			}

			if (clazz != null)
			{
				Object obj = task.getReference(reference);
				if (obj == null)
				{
					return;
				}

				Class objectClass = obj.getClass();
				if (!objectClass.getName().equals(clazz.getName()) && !ReflectionHelper.extendsClass(objectClass, clazz))
				{
					throw new BuildException("Class '" + clazz.getName() + "' specified for the parameter " +
							"referenced by '" + reference + "' doesn't match real class of the referenced object");
				}
			}

			return;
		}

		if (clazz == null || String.class.equals(clazz))
		{
			return;
		}

		String primaryClassName = clazz.isArray() ? clazz.getComponentType().getName() : clazz.getName();

		try
		{
			boolean hasConstructor = ReflectionHelper.hasConstructor(primaryClassName, new Class[] {String.class});
			if (!hasConstructor)
			{
				throw new BuildException(primaryClassName + " class doesn't have constructor receiving String as an argument");
			}
		}
		catch (ClassNotFoundException e)
		{
			throw new BuildException("Failed to check if " + primaryClassName + " class has constructor receiving String as an argument");
		}
	}

	protected final Class getClazz()
	{
		return clazz;
	}
}
