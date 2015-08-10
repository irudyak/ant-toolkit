package com.anttoolkit.ldap.tasks;

import java.util.*;
import javax.naming.*;
import javax.naming.directory.*;

import org.apache.tools.ant.*;

public class SearchTask
		extends GenericSearchTask
		implements TaskContainer
{
	private List<Task> tasks = new LinkedList<Task>();

	private static ThreadLocal<Stack<SearchResult>> currentResult = new ThreadLocal<Stack<SearchResult>>()
	{
		protected Stack<SearchResult> initialValue()
		{
			return new Stack<SearchResult>();
		}
	};

	static Attribute getSearchResultAttribute(String attrName)
	{
		if (currentResult.get() == null || currentResult.get().isEmpty())
		{
			throw new BuildException("There are no LDAP search executed to return any results");
		}

		return currentResult.get().peek().getAttributes().get(attrName);
	}

	@Override
	public void addTask(Task task)
	{
		tasks.add(task);
	}

	@Override
	public void doWork() throws BuildException
	{
		NamingEnumeration<SearchResult> result = search();

		try
		{
			while (result.hasMore())
			{
				currentResult.get().push(result.next());

				for (Task task : tasks)
				{
					task.perform();
				}
			}
		}
		catch (NamingException e)
		{
			if (e instanceof SizeLimitExceededException && this.countLimitSpecified())
			{
				return;
			}

			throw new BuildException("Failed to iterate through LDAP search results", e);
		}
	}
}
