package com.anttoolkit.general.tasks.concurrent;

import com.anttoolkit.general.tasks.GenericTask;
import org.apache.tools.ant.*;

import com.anttoolkit.general.tasks.concurrent.util.*;

public class CreateCyclicBarrierTask
		extends GenericTask
{
	private String name = null;
	private int parties = -1;

	public void setName(String name)
	{
		this.name = name;
	}

	public void setParties(int parties)
	{
		this.parties = parties;
	}

	public void doWork() throws BuildException
	{
		ThreadManager.createCyclicBarrier(name, parties);
	}

	protected void validate()
	{
		if (name == null)
		{
			throw new BuildException("Barrier name should be specified");
		}

		if (parties <= 0)
		{
			throw new BuildException("Parties value should be specified greater than zero");
		}
	}
}
