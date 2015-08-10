package com.anttoolkit.general.tasks.concurrent.util;

import java.util.*;

public class ThreadsComparator implements Comparator<Thread>
{
	public static final ThreadsComparator INSTANCE = new ThreadsComparator();

	private ThreadsComparator()
	{}

	public int compare(Thread thread1, Thread thread2)
	{
		String name1 = thread1.getName() != null ? thread1.getName() : Long.toString(thread1.getId());
		String name2 = thread2.getName() != null ? thread2.getName() : Long.toString(thread2.getId());

		return name1.compareTo(name2);
	}
}
