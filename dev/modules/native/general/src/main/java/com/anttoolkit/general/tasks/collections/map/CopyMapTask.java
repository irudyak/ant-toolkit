package com.anttoolkit.general.tasks.collections.map;

import org.apache.tools.ant.*;

import com.anttoolkit.general.tasks.*;
import com.anttoolkit.general.tasks.collections.map.util.*;

public class CopyMapTask
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
		MapManager.copy(src, dest);
	}

	@Override
	protected void validate()
	{
		if (src == null || src.trim().isEmpty())
		{
			throw new BuildException("Source map should be specified");
		}

		if (dest == null || dest.trim().isEmpty())
		{
			throw new BuildException("Destination map should be specified");
		}

		if (!MapManager.exists(src))
		{
			throw new BuildException("Source map doesn't exist");
		}
	}
}
