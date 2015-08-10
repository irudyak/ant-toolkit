package com.anttoolkit.general.tasks.reflection;

import org.apache.tools.ant.*;

import com.anttoolkit.general.tasks.*;

public class GetObjectArrayLengthTask extends GenericTask
{
	private String arrayRef;
	private String property;

	public void setArrayReference(String arrayRef)
	{
		this.arrayRef = arrayRef;
	}

	public void setProperty(String property)
	{
		this.property = property;
	}

	@Override
	public void doWork() throws BuildException
	{
		int length = ((Object[])this.getReference(arrayRef)).length;
		this.setPropertyThreadSafe(property, Integer.toString(length));
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

		if (property == null)
		{
			throw new BuildException("Property name should be specified");
		}
	}
}
