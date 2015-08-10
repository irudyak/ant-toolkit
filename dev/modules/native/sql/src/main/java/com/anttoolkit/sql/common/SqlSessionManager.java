package com.anttoolkit.sql.common;

import java.util.*;

import org.apache.tools.ant.*;

public class SqlSessionManager
{
	private static ThreadLocal<Map<String, SqlSession>> sessions = new ThreadLocal<Map<String, SqlSession>>()
	{
		protected Map<String, SqlSession> initialValue()
		{
			return new HashMap<String, SqlSession>();
		}
	};

	private static ThreadLocal<Stack<String>> loginInfoStack = new ThreadLocal<Stack<String>>()
	{
		protected Stack<String> initialValue()
		{
			return new Stack<String>();
		}
	};

	public static SqlSession getSession()
	{
		try
		{
			return sessions.get().get(loginInfoStack.get().peek());
		}
		catch (EmptyStackException e)
		{
			throw new BuildException("Failed to get session - login info wasn't specified");
		}
	}

	public static SqlSession newSession(LoginInfo loginInfo)
	{
		return new SqlSession(loginInfo);
	}

	public static boolean hasSession()
	{
		try
		{
			return getSession() != null;
		}
		catch (Throwable e)
		{
			return false;
		}
	}

	public static void setCurrentSessionContext(LoginInfo loginInfo)
	{
		if (!sessions.get().containsKey(loginInfo.toString()))
		{
			sessions.get().put(loginInfo.toString(), new SqlSession(loginInfo));
		}

		loginInfoStack.get().push(loginInfo.toString());
	}

	public static void releaseCurrentSessionContext()
	{
		String loginInfo = loginInfoStack.get().pop();

		if (!loginInfoStack.get().contains(loginInfo))
		{
			SqlSession session = sessions.get().remove(loginInfo);
			session.closeConnection();
		}
	}
}
