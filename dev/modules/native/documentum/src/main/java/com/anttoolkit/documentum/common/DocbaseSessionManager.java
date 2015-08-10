package com.anttoolkit.documentum.common;

import org.apache.tools.ant.*;

import java.util.*;

public class DocbaseSessionManager
{
	private static ThreadLocal<Map<String, DocbaseSession>> sessions = new ThreadLocal<Map<String, DocbaseSession>>()
	{
		protected Map<String, DocbaseSession> initialValue()
		{
			return new HashMap<String, DocbaseSession>();
		}
	};

	private static ThreadLocal<Stack<String>> loginInfoStack = new ThreadLocal<Stack<String>>()
	{
		protected Stack<String> initialValue()
		{
			return new Stack<String>();
		}
	};

	public static DocbaseSession getSession()
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

	public static DocbaseSession newSession(LoginInfo loginInfo)
	{
		return new DocbaseSession(loginInfo);
	}

	public static void setCurrentSessionContext(LoginInfo loginInfo)
	{
		if (!sessions.get().containsKey(loginInfo.toString()))
		{
			sessions.get().put(loginInfo.toString(), new DocbaseSession(loginInfo));
		}

		loginInfoStack.get().push(loginInfo.toString());
	}

	public static void releaseCurrentSessionContext()
	{
		String loginInfo = loginInfoStack.get().pop();

		if (!loginInfoStack.get().contains(loginInfo))
		{
			DocbaseSession session = sessions.get().remove(loginInfo);
			session.releaseSession();
		}
	}

	public static LoginInfo getCurrentSessionContext()
	{
		String loginString = loginInfoStack.get().peek();
		return loginString == null ? null : new LoginInfo(loginString);
	}
}
