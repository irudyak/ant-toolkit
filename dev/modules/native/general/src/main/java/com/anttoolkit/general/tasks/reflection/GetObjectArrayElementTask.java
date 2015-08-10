package com.anttoolkit.general.tasks.reflection;

import org.apache.tools.ant.*;

import com.anttoolkit.general.tasks.*;

public class GetObjectArrayElementTask extends GenericTask
{
	private String arrayRef;
	private Integer index;
	private String property;
	private String reference;

	public void setArrayReference(String arrayRef)
	{
		this.arrayRef = arrayRef;
	}

	public void setIndex(int index)
	{
		this.index = index;
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
		Object obj = ((Object[])this.getReference(arrayRef))[index];

		if (property != null)
		{
			this.setPropertyThreadSafe(property, obj == null ? null : obj.toString());
		}

		if (reference != null)
		{
			this.setReference(reference, obj);
		}
	}

	protected void validate()
	{
		if (arrayRef == null || arrayRef.trim().isEmpty())
		{
			throw new BuildException("Array reference should be specified");
		}

		if (this.checkReferenceExists(arrayRef))
		{
			throw new BuildException("Specified array reference " + arrayRef + " doesn't exist");
		}

		if (!this.getReference(arrayRef).getClass().isArray())
		{
			throw new BuildException("Specified array reference " + arrayRef + " contains not array object of type " + this.getReference(arrayRef).getClass().getName());
		}

		if (index == null || index < 0)
		{
			throw new BuildException("Incorrect array element index " + index + " specified");
		}

		if (((Object[])this.getReference(arrayRef)).length <= index)
		{
			throw new BuildException("Specified index " + index + " is out of the array '" + arrayRef + "' index range");
		}

		if (property == null && reference == null)
		{
			throw new BuildException("Property or reference name should be specified");
		}
	}
}
