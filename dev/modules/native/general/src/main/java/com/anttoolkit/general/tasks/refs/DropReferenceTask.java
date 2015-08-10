package com.anttoolkit.general.tasks.refs;

import org.apache.tools.ant.*;

import com.anttoolkit.general.tasks.*;

public class DropReferenceTask extends GenericTask
{
	private String ref = null;

	public void setRefid(String ref)
	{
		this.ref = ref;
	}

	@Override
	public void doWork() throws BuildException
	{
		this.removeReference(ref);
	}

	protected void validate()
	{
		if (ref == null)
		{
			throw new BuildException("Reference id should be specified");
		}
	}
}
