package com.anttoolkit.general.tasks.build.util;

import java.util.*;

import com.anttoolkit.general.tasks.*;

public class BuildContextManager
{
	private static final String VERSION_PROVIDER_REF = "anttoolkit.build.version.provider";

	private static ThreadLocal<Stack<String>> qualifierStack = new ThreadLocal<Stack<String>>()
	{
		protected Stack<String> initialValue()
		{
			return new Stack<String>();
		}
	};

	public static void registerVersionProvider(GenericTask task, IBuildVersionProvider provider)
	{
		if (provider == null)
		{
			throw new IllegalArgumentException("Can't register null version provider");
		}

		task.setReference(VERSION_PROVIDER_REF, provider);
	}

	public static IBuildVersionProvider getVersionProvider(GenericTask task)
	{
		Object obj = task.getReference(VERSION_PROVIDER_REF);

		if (obj == null)
		{
			return null;
		}

		if (!(obj instanceof IBuildVersionProvider))
		{
			throw new IllegalStateException("Incorrect object registered as version provider");
		}

		return (IBuildVersionProvider)obj;
	}

	public static void setContextQualifier(String qualifier)
	{
		qualifierStack.get().push(qualifier);
	}

	public static String getContextQualifier()
	{
		try
		{
			return qualifierStack.get().peek();
		}
		catch (EmptyStackException e)
		{
			return "";
		}
	}

	public static void releaseContextQualifier()
	{
		try
		{
			qualifierStack.get().pop();
		}
		catch (EmptyStackException e) {}
	}
}
