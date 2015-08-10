package com.anttoolkit.zookeeper.common;

import java.util.*;

import com.anttoolkit.zookeeper.types.*;

public class ZookeeperSessionManager
{
	private static ThreadLocal<Stack<ZookeeperSession>> sessionsStack = new ThreadLocal<Stack<ZookeeperSession>>()
	{
		protected Stack<ZookeeperSession> initialValue()
		{
			return new Stack<ZookeeperSession>();
		}
	};

	public static void setCurrentContext(ZookeeperConfig config)
	{
		sessionsStack.get().push(new ZookeeperSession(config));
	}

	public static ZookeeperSession getZookeeperSession()
	{
		try
		{
			return sessionsStack.get().peek();
		}
		catch (EmptyStackException e)
		{
			return null;
		}
	}

	public static void releaseCurrentContext()
	{
		try
		{
			ZookeeperSession session = sessionsStack.get().pop();
			if (session != null)
			{
				session.close();
			}
		}
		catch (EmptyStackException e) {}
	}
}
