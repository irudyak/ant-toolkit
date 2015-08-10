package com.anttoolkit.maestro.common;

import java.util.*;

import org.apache.tools.ant.*;

import com.anttoolkit.maestro.types.*;
import com.anttoolkit.general.tasks.*;

public class MaestroSessionManager
{
	private static ThreadLocal<Stack<MaestroConfig>> configsStack = new ThreadLocal<Stack<MaestroConfig>>()
	{
		protected Stack<MaestroConfig> initialValue()
		{
			return new Stack<MaestroConfig>();
		}
	};

	public static void setCurrentContext(MaestroConfig config)
	{
		configsStack.get().push(config);
	}

	public static String getUserId(GenericTask task)
	{
		try
		{
			MaestroConfig conf = configsStack.get().peek();
			return conf.getUserId(task);
		}
		catch (EmptyStackException e)
		{
			String userId = MaestroHelper.getDefaultUsedId(task);
			if (userId != null)
			{
				return userId;
			}

			throw new BuildException("There are no Maestro server configurations specified to connect to");
		}
	}

	public static String getToken(GenericTask task)
	{
		try
		{
			MaestroConfig conf = configsStack.get().peek();
			return conf.getToken(task);
		}
		catch (EmptyStackException e)
		{
			String token = MaestroHelper.getDefaultToken(task);
			if (token != null)
			{
				return token;
			}

			throw new BuildException("There are no Maestro server configurations specified to connect to");
		}
	}

	public static void releaseCurrentContext()
	{
		try
		{
			configsStack.get().pop();
		}
		catch (EmptyStackException e) {}
	}
}
