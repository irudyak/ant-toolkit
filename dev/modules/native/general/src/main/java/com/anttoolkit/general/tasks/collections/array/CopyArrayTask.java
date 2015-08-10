package com.anttoolkit.general.tasks.collections.array;

import org.apache.tools.ant.*;

import com.anttoolkit.general.tasks.*;
import com.anttoolkit.general.tasks.collections.array.util.*;

public class CopyArrayTask
		extends GenericTask
{
	private String src;
	private String dest;

	public void setSrc(String src)
	{
		this.src = src;
	}

	public void setDest(String dest)
	{
		this.dest = dest;
	}

	@Override
	public void doWork() throws BuildException
	{
		ArrayManager.copy(src, dest);
	}

	@Override
	protected void validate()
	{
		if (src == null || src.trim().isEmpty())
		{
			throw new BuildException("Source array should be specified");
		}

		if (dest == null || dest.trim().isEmpty())
		{
			throw new BuildException("Destination array should be specified");
		}

		if (!ArrayManager.exists(src))
		{
			throw new BuildException("Source array doesn't exist");
		}
	}
}
