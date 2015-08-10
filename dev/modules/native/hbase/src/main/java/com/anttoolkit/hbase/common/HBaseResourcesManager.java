package com.anttoolkit.hbase.common;

import java.util.*;

import org.apache.hadoop.conf.*;

public class HBaseResourcesManager
{
	private static ThreadLocal<Stack<HBaseResourcesProvider>> providerStack = new ThreadLocal<Stack<HBaseResourcesProvider>>()
	{
		protected Stack<HBaseResourcesProvider> initialValue()
		{
			return new Stack<HBaseResourcesProvider>();
		}
	};

	public static HBaseResourcesProvider setCurrentContext(Configuration conf)
	{
		HBaseResourcesProvider provider = new HBaseResourcesProvider(conf);
		providerStack.get().push(provider);
		return provider;
	}

	public static HBaseResourcesProvider getProvider()
	{
		try
		{
			return providerStack.get().peek();
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
			HBaseResourcesProvider provider = providerStack.get().pop();
			if (provider != null)
			{
				provider.releaseResources();
			}
		}
		catch (EmptyStackException e) {}
	}
}
