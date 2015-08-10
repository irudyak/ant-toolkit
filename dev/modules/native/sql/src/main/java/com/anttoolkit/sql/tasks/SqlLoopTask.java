package com.anttoolkit.sql.tasks;

import java.sql.*;
import java.util.*;

import com.anttoolkit.general.tasks.collections.array.util.ArrayManager;
import com.anttoolkit.general.tasks.GenericTask;
import org.apache.tools.ant.*;

import com.anttoolkit.sql.common.*;

public class SqlLoopTask
		extends GenericTask
		implements TaskContainer
{
	private String query = null;
	private String delimiter = ";";
	private String columnsArray = null;
	private List<Task> tasks = new LinkedList<Task>();

	private static ThreadLocal<Stack<ResultSet>> currentResultSet = new ThreadLocal<Stack<ResultSet>>()
	{
		protected Stack<ResultSet> initialValue()
		{
			return new Stack<ResultSet>();
		}
	};

	static String getCurrentRowColumn(String columnName, String format)
	{
		if (currentResultSet.get() == null || currentResultSet.get().isEmpty())
		{
			throw new BuildException("There are no SQL query executed to return any column values");
		}

		ResultSet resultSet = currentResultSet.get().peek();

		return SqlHelper.getColumnValue(resultSet, columnName, format);
	}

	public void setQuery(String query)
	{
		this.query = query.trim();
	}

	public void setDelimiter(String delimiter)
	{
		this.delimiter = delimiter;
	}

	public void setColumnsArray(String array)
	{
		columnsArray = array;
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

		ResultSet resultSet = null;
		try
		{
			resultSet = SqlHelper.executeQuery(SqlSessionManager.getSession(), query, delimiter);
			if (resultSet == null)
			{
				return;
			}

			initColumnsArray(resultSet);

			currentResultSet.get().push(resultSet);

			while (resultSet.next())
			{
				for (Task task : tasks)
				{
					task.perform();
				}
			}

			currentResultSet.get().pop();
		}
		catch (SQLException e)
		{
			throw new BuildException("Exception occured while trying to iterate through result set", e);
		}
		finally
		{
			SqlHelper.closeResultSet(resultSet);
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

	private void initColumnsArray(ResultSet resultSet)
	{
		if (columnsArray == null)
		{
			return;
		}

		List<String> names = SqlHelper.getColumnNames(resultSet);
		ArrayManager.init(columnsArray, names, false);
	}
}
