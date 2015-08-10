package com.anttoolkit.zookeeper.tasks.util;

import java.util.*;

import org.apache.tools.ant.*;

public class SyncPrimitivesManager
{
	private static Map<String, CyclicBarrier> cyclicBarriers = new HashMap<String, CyclicBarrier>();

	public static void addCyclicBarrier(String name, CyclicBarrier barrier)
	{
		synchronized (cyclicBarriers)
		{
			cyclicBarriers.put(name, barrier);
		}
	}

	public static CyclicBarrier getCyclicBarrier(String name)
	{
		synchronized (cyclicBarriers)
		{
			CyclicBarrier barrier = cyclicBarriers.get(name);
			if (barrier == null)
			{
				throw new BuildException("Cyclic barrier '" + name + "' wasn't previously registered");
			}

			return barrier;
		}
	}

	public static CyclicBarrier removeCyclicBarrier(String name)
	{
		synchronized (cyclicBarriers)
		{
			return cyclicBarriers.remove(name);
		}
	}
}
