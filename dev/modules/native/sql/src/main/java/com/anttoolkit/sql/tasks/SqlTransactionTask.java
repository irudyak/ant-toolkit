package com.anttoolkit.sql.tasks;

import java.sql.*;
import java.util.*;

import com.anttoolkit.general.tasks.GenericTask;
import org.apache.tools.ant.*;

import com.anttoolkit.sql.common.*;

public class SqlTransactionTask
		extends GenericTask
		implements TaskContainer
{
	private static final String READ_COMMITTED = "READ_COMMITTED";
	private static final String REPEATABLE_READ = "REPEATABLE_READ";
	private static final String SERIALIZABLE = "SERIALIZABLE";

	private int isolationLevel = Connection.TRANSACTION_READ_COMMITTED;
	private List<Task> tasks = new LinkedList<Task>();

	public void setIsolationLevel(String level)
	{
		if (READ_COMMITTED.equals(level))
		{
			isolationLevel = Connection.TRANSACTION_READ_COMMITTED;
		}
		else if (REPEATABLE_READ.equals(level))
		{
			isolationLevel = Connection.TRANSACTION_REPEATABLE_READ;
		}
		else if (SERIALIZABLE.equals(level))
		{
			isolationLevel = Connection.TRANSACTION_SERIALIZABLE;
		}
		else
		{
			throw new BuildException("Incorrect transaction isolation level specified: " + level);
		}
	}

	public void doWork() throws BuildException
	{
		if (tasks.isEmpty())
		{
			return;
		}

		try
		{
			SqlSessionManager.getSession().beginTransaction(isolationLevel);

			for (Task task : tasks)
			{
				task.perform();
			}

			SqlSessionManager.getSession().commitTransaction();
		}
		catch (Throwable e)
		{
			SqlSessionManager.getSession().rollbackTransaction();

			if (e instanceof BuildException)
			{
				throw (BuildException)e;
			}

			throw new BuildException("Exception occured", e);
		}
	}

	public void addTask(Task task)
	{
		tasks.add(task);
	}
}
