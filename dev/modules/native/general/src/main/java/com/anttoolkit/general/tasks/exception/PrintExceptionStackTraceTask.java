package com.anttoolkit.general.tasks.exception;

import org.apache.tools.ant.*;

import com.anttoolkit.general.common.*;
import com.anttoolkit.general.tasks.*;

public class PrintExceptionStackTraceTask
		extends GenericTask
{
	private String ref;

	public void setRefId(String ref)
	{
		this.ref = ref;
	}

	@Override
	public void doWork() throws BuildException
	{
		String trace = ExceptionHelper.stackTraceToString((Throwable)this.getReference(ref));
		this.getProject().log(trace);
	}

	protected void validate()
	{
		if (ref == null)
		{
			throw new BuildException("Reference wasn't specified");
		}

		Object obj = this.getReference(ref);
		if (obj == null || !(obj instanceof Throwable))
		{
			throw new BuildException("Specified reference '" + ref + "' doesn't contain exception object");
		}
	}
}
