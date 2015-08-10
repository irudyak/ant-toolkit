package com.anttoolkit.documentum.common;

import java.util.*;
import java.util.concurrent.locks.*;

public class SynchronizationManager
{
	private static final int LOCKS_POOL_SIZE = 100;

	private static volatile Stack<ReentrantLock> locksPool = new Stack<ReentrantLock>();
	private static volatile Map<Object, ReentrantLock> objectLocksMap = new HashMap<Object, ReentrantLock>();

	private static volatile List<Object> tryingToLockObjects = new Vector<Object>();

	public static void lock(Object obj)
	{
		if (obj == null)
		{
			return;
		}

		ReentrantLock lock = null;

		synchronized (objectLocksMap)
		{
			if (!objectLocksMap.containsKey(obj))
			{
				lock = getNewLock();
				objectLocksMap.put(obj, lock);
				lock.lock();

				return;
			}

			lock = objectLocksMap.get(obj);
			if (lock.isHeldByCurrentThread())
			{
				lock.lock();
				return;
			}

			if (lock.tryLock())
			{
				return;
			}

			tryingToLockObjects.add(obj);
		}

		try
		{
			lock.lock();
		}
		finally
		{
			tryingToLockObjects.remove(obj);
		}
	}

	public static void unlock(Object obj)
	{
		if (obj == null)
		{
			return;
		}

		synchronized (objectLocksMap)
		{
			ReentrantLock lock = objectLocksMap.get(obj);
			if (lock == null || !lock.isHeldByCurrentThread())
			{
				return;
			}

			if (!lock.hasQueuedThreads() &&
				lock.getHoldCount() == 1 &&
				!tryingToLockObjects.contains(obj))
			{
				objectLocksMap.remove(obj);
				returnToPool(lock);
			}

			lock.unlock();
		}
	}

	public static boolean isLocked(Object obj)
	{
		if (obj == null)
		{
			return false;
		}

		synchronized (objectLocksMap)
		{
			ReentrantLock lock = objectLocksMap.get(obj);
			return lock != null && lock.isLocked();
		}
	}

	public static boolean isLockedByCurrentThread(Object obj)
	{
		if (obj == null)
		{
			return false;
		}

		synchronized (objectLocksMap)
		{
			ReentrantLock lock = objectLocksMap.get(obj);
			return lock != null && lock.isHeldByCurrentThread();
		}
	}

	private static ReentrantLock getNewLock()
	{
		return locksPool.empty() ? new ReentrantLock() : locksPool.pop();
	}

	private static void returnToPool(ReentrantLock lock)
	{
		if (locksPool.size() <= LOCKS_POOL_SIZE)
		{
			locksPool.push(lock);
		}
	}
}
