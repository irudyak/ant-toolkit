package com.anttoolkit.general.tasks.concurrent;

import org.apache.tools.ant.*;

import com.anttoolkit.general.tasks.*;
import com.anttoolkit.general.tasks.concurrent.util.*;

public class WaitThreadTask
		extends GenericTask
{
	private String threadGroups = null;
	private boolean failOnAny = false;
	private boolean forbidNewThreadsOnFailure = true;

	public void setGroup(String groups)
	{
		threadGroups = groups;
	}

	public void setFailonany(boolean fail)
	{
		failOnAny = fail;
	}

	public void setForbidNewThreadsOnFailure(boolean forbid)
	{
		forbidNewThreadsOnFailure = forbid;
	}

	public void doWork() throws BuildException
	{
		String[] groups = threadGroups.split(",", -1);
		for (String group : groups)
		{
			ThreadManager.waitThreadsToComplete(this, group, failOnAny, forbidNewThreadsOnFailure);
		}
	}

	@Override
	protected void validate()
	{
		if (threadGroups == null || threadGroups.trim().isEmpty())
		{
			throw new BuildException("Thread group should be specified");
		}
	}
}
