package com.anttoolkit.general.tasks.reflection.util;

import org.apache.tools.ant.*;

import com.anttoolkit.general.tasks.*;

public class NullParameter extends Parameter
{
	@Override
	public Object getParamValue(GenericTask task)
	{
		return null;
	}

	public void validate(GenericTask task)
	{
		if (getClazz() == null)
		{
			throw new BuildException("Parameter class should be specified");
		}
	}
}
