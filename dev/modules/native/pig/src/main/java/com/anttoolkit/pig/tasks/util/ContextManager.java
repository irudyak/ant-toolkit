package com.anttoolkit.pig.tasks.util;

import java.util.*;

import org.apache.pig.data.*;

public class ContextManager
{
	private static ThreadLocal<Stack<Tuple>> tupleStack = new ThreadLocal<Stack<Tuple>>()
	{
		protected Stack<Tuple> initialValue()
		{
			return new Stack<Tuple>();
		}
	};

	private static ThreadLocal<Stack<DataBag>> bagStack = new ThreadLocal<Stack<DataBag>>()
	{
		protected Stack<DataBag> initialValue()
		{
			return new Stack<DataBag>();
		}
	};

	private static ThreadLocal<Stack<Map<String, Object>>> mapStack = new ThreadLocal<Stack<Map<String, Object>>>()
	{
		protected Stack<Map<String, Object>> initialValue()
		{
			return new Stack<Map<String, Object>>();
		}
	};

	public static Tuple getCurrentTuple()
	{
		if (tupleStack.get() == null || tupleStack.get().isEmpty())
		{
			return null;
		}

		return tupleStack.get().peek();
	}

	public static void setCurrentTupleContext(Tuple tuple)
	{
		tupleStack.get().push(tuple);
	}

	public static void resetCurrentTupleContext()
	{
		try
		{
			tupleStack.get().pop();
		}
		catch (EmptyStackException e) {}
	}

	public static DataBag getCurrentBag()
	{
		if (bagStack.get() == null || bagStack.get().isEmpty())
		{
			return null;
		}

		return bagStack.get().peek();
	}

	public static void setCurrentBagContext(DataBag bag)
	{
		bagStack.get().push(bag);
	}

	public static void resetCurrentBagContext()
	{
		try
		{
			bagStack.get().pop();
		}
		catch (EmptyStackException e) {}
	}

	public static Map<String, Object> getCurrentMap()
	{
		if (mapStack.get() == null || mapStack.get().isEmpty())
		{
			return null;
		}

		return mapStack.get().peek();
	}

	public static void setCurrentMapContext(Map<String, Object> map)
	{
		mapStack.get().push(map);
	}

	public static void resetCurrentMapContext()
	{
		try
		{
			mapStack.get().pop();
		}
		catch (EmptyStackException e) {}
	}

}
