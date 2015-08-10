package com.anttoolkit.documentum.tasks.dql;

import java.util.*;

import com.anttoolkit.general.tasks.collections.array.util.ArrayManager;
import org.apache.tools.ant.*;

import com.anttoolkit.documentum.common.*;

import com.documentum.fc.client.*;
import com.documentum.fc.common.*;

public class DqlLoopTask
		extends GenericDocbaseTask
		implements TaskContainer
{
	private String query = null;
	private String columnNamesArray = null;
	private List<Task> tasks = new LinkedList<Task>();
	private static ThreadLocal<Stack<IDfTypedObject>> currentRow = new ThreadLocal<Stack<IDfTypedObject>>()
	{
		protected Stack<IDfTypedObject> initialValue()
		{
			return new Stack<IDfTypedObject>();
		}
	};

	static String getCurrentRowColumn(String columnName, String format)
	{
		if (currentRow.get() == null || currentRow.get().isEmpty())
		{
			throw new BuildException("There are no dql query executed to return any column values");
		}

		return DocbaseObjectsHelper.getAttributeValueAsString(currentRow.get().peek(), columnName, format);
	}

	public void setQuery(String query)
	{
		this.query = query.trim();
	}

	public void setColumnNamesArray(String array)
	{
		columnNamesArray = array;
	}

	public void addTask(Task task)
	{
		tasks.add(task);
	}

	public void doWork()
			throws BuildException
	{
		if (tasks.size() == 0)
		{
			return;
		}

		IDfCollection coll = null;
		try
		{
			coll = DqlHelper.executeReadQuery(getSession(), query);
			if (coll == null)
			{
				return;
			}

			initColumnsArray(coll);

			while (coll.next())
			{
				currentRow.get().push(coll.getTypedObject());

				for (Task task : tasks)
				{
					task.perform();
				}

				currentRow.get().pop();
			}
		}
		catch (DfException e)
		{
			throw new BuildException("Exception occured during dql loop", e);
		}
		finally
		{
			DqlHelper.closeCollection(coll);
		}
	}

	protected void validate()
	{
		if (query == null)
		{
			throw new BuildException("Query should be specified");
		}

		String _query = query.toUpperCase();
		if (_query.indexOf("SELECT") != 0)
		{
			throw new BuildException("Invalid query type. Only SELECT like queries could be used.");
		}
	}

	private void initColumnsArray(IDfCollection coll)
	{
		if (columnNamesArray == null)
		{
			return;
		}

		try
		{
			int count = coll.getAttrCount();
			for (int i = 0; i < count; i++)
			{
				IDfAttr attr = coll.getAttr(i);
				ArrayManager.add(columnNamesArray, attr.getName());
			}
		}
		catch (DfException e)
		{
			throw new BuildException("Failed to get query columns info");
		}
	}
}
