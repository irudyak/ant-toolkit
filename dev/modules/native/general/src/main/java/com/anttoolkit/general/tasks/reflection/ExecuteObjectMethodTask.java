package com.anttoolkit.general.tasks.reflection;

import org.apache.tools.ant.*;

public class ExecuteObjectMethodTask extends MethodInvocationTask
{
	private String reference;

	public void setObjectReference(String reference)
	{
		this.reference = reference;
	}

	@Override
	protected Object getObject()
	{
		return getReference(reference);
	}

	@Override
	protected void validate()
	{
		super.validate();

		if (reference == null || reference.trim().isEmpty())
		{
			throw new BuildException("Reference should be specified");
		}

		if (!checkReferenceExists(reference))
		{
			throw new BuildException("Specified reference '" + reference + "' doesn't exist");
		}
	}

}
