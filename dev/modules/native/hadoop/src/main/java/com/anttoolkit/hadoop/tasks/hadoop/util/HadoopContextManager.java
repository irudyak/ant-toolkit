package com.anttoolkit.hadoop.tasks.hadoop.util;

import java.util.*;

import com.anttoolkit.hadoop.types.*;

public class HadoopContextManager
{
	private static ThreadLocal<Stack<HadoopConfig>> configStack = new ThreadLocal<Stack<HadoopConfig>>()
	{
		protected Stack<HadoopConfig> initialValue()
		{
			return new Stack<HadoopConfig>();
		}
	};

	private static ThreadLocal<Stack<AuthenticationContext>> authStack = new ThreadLocal<Stack<AuthenticationContext>>()
	{
		protected Stack<AuthenticationContext> initialValue()
		{
			return new Stack<AuthenticationContext>();
		}
	};

	private static ThreadLocal<Stack<String>> defaultFsStack = new ThreadLocal<Stack<String>>()
	{
		protected Stack<String> initialValue()
		{
			return new Stack<String>();
		}
	};

	public static void setConfigContext(HadoopConfig config)
	{
		configStack.get().push(config);
	}

	public static void releaseConfigContext()
	{
		try
		{
			configStack.get().pop();
		}
		catch (EmptyStackException e)
		{
		}
	}

	public static HadoopConfig getConfigContext()
	{
		try
		{
			return configStack.get().peek();
		}
		catch (EmptyStackException e)
		{
			return null;
		}
	}

	public static void setAuthenticationContext(AuthenticationContext authContext)
	{
		if (authContext == null)
		{
			throw new IllegalArgumentException("Authentication context can't be null");
		}

		AuthenticationContext currentAuthContext = getAuthenticationContext();

		//if provided context equals to current context we just simply add the same reference to the stack,
		//to prevent creation of several nested (and absolutely the same) security context,
		//during privileged execion execution
		authStack.get().push(authContext.equals(currentAuthContext) ? currentAuthContext : authContext);
	}

	public static void releaseAuthenticationContext()
	{
		try
		{
			authStack.get().pop();
		}
		catch (EmptyStackException e)
		{
		}
	}

	public static AuthenticationContext getAuthenticationContext()
	{
		try
		{
			return authStack.get().peek();
		}
		catch (EmptyStackException e)
		{
			return null;
		}
	}

	public static void setFilesystemContext(String defaultFs)
	{
		defaultFsStack.get().push(defaultFs);
	}

	public static void releaseFilesystemContext()
	{
		try
		{
			defaultFsStack.get().pop();
		}
		catch (EmptyStackException e)
		{
		}
	}

	public static String getFilesystemContext()
	{
		try
		{
			return defaultFsStack.get().peek();
		}
		catch (EmptyStackException e)
		{
			return null;
		}
	}
}
