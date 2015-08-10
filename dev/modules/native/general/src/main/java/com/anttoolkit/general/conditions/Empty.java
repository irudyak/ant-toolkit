package com.anttoolkit.general.conditions;

import org.apache.tools.ant.*;
import org.apache.tools.ant.taskdefs.condition.*;

public class Empty implements Condition
{
	private String str;
	private boolean trim = false;

	public void setString(String str)
	{
		this.str = str;
	}

	public void setTrim(boolean trim)
	{
		this.trim = trim;
	}

	@Override
	public boolean eval() throws BuildException
	{
		validate();
		return trim ? str.trim().isEmpty() : str.isEmpty();
	}

	protected void validate()
	{
		if (str == null)
		{
			throw new BuildException("String to check for emptyness should be specified");
		}
	}
}
