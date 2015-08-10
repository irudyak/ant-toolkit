package com.anttoolkit.general.tasks.refs;

import org.apache.tools.ant.*;

import com.anttoolkit.general.tasks.*;

public class CheckReferenceIsNullTask extends GenericTask
{
	private String reference;
	private String property;

	public void setReference(String reference)
	{
		this.reference = reference;
	}

	public void setProperty(String property)
	{
		this.property = property;
	}

	@Override
	public void doWork() throws BuildException
	{
		Object obj = this.getReference(reference);
		this.setPropertyThreadSafe(property, obj == null ? Boolean.TRUE.toString() : Boolean.FALSE.toString());
	}

	@Override
	protected void validate()
	{
		if (reference == null || reference.trim().isEmpty())
		{
			throw new BuildException("Reference should be specified");
		}

		if (property == null || property.isEmpty())
		{
			throw new BuildException("Property should be specified");
		}

		if (!this.checkReferenceExists(reference))
		{
			throw new BuildException("Specified reference '" + reference + "' doesn't exist");
		}
	}
}
