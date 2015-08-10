package com.anttoolkit.zookeeper.tasks;

import org.apache.tools.ant.*;
import org.apache.zookeeper.*;

import com.anttoolkit.zookeeper.tasks.util.*;

public class DeleteCyclicBarrierTask
		extends GenericZookeeperTask
{
	private String name;

	public void setName(String name)
	{
		this.name = name;
	}

	@Override
	public void doWork() throws BuildException
	{
		CyclicBarrier barrier = SyncPrimitivesManager.removeCyclicBarrier(name);
		if (barrier == null)
		{
			throw new BuildException("There is no registered cyclic barrier with name '" + name + "'");
		}

		try
		{
			getZookeeperSession().delete(barrier.barrierNode, true, -1);
		}
		catch (BuildException e)
		{
			if (!(e.getCause() instanceof KeeperException.NoNodeException))
			{
				throw new BuildException("Failed to delete '" + barrier.barrierNode + "' cyclic barrier", e);
			}
		}
	}

	@Override
	protected void validate()
	{
		if (name == null || name.trim().isEmpty())
		{
			throw new BuildException("Cyclic barrier name should be specified");
		}
	}
}
