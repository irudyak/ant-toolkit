package com.anttoolkit.general.tasks.collections.queue;

import org.apache.tools.ant.*;

import com.anttoolkit.general.tasks.collections.queue.util.*;
import com.anttoolkit.general.tasks.*;

public class CopyQueueTask extends GenericTask
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
		QueueManager.copy(src, dest);
	}

	@Override
	protected void validate()
	{
		if (src == null || src.trim().isEmpty())
		{
			throw new BuildException("Source queue should be specified");
		}

		if (dest == null || dest.trim().isEmpty())
		{
			throw new BuildException("Destination queue should be specified");
		}

		if (!QueueManager.exists(src))
		{
			throw new BuildException("Source queue doesn't exist");
		}
	}
}
