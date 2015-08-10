package com.anttoolkit.hbase.tasks.data.util;

import java.util.*;

import com.anttoolkit.general.common.*;
import org.apache.hadoop.hbase.client.*;
import org.apache.tools.ant.*;

import com.anttoolkit.hbase.tasks.*;

public class BatchAction
{
	private GenericHBaseTask task;
	private List<IAction> actions = new LinkedList<IAction>();

	public BatchAction(GenericHBaseTask task)
	{
		this.task = task;
	}

	public DeleteAction createDelete()
	{
		DeleteAction delete = new DeleteAction(task);
		actions.add(delete);
		return delete;
	}

	public PutAction createPut()
	{
		PutAction put = new PutAction(task);
		actions.add(put);
		return put;
	}

	public GetAction createGet()
	{
		GetAction get = new GetAction(task);
		actions.add(get);
		return get;
	}

	public IncrementAction createIncrement()
	{
		IncrementAction inc = new IncrementAction(task);
		actions.add(inc);
		return inc;
	}

	public AppendAction createAppend()
	{
		AppendAction append = new AppendAction(task);
		actions.add(append);
		return append;
	}

// Row mutation operations are not supported for batch execution in HBase release 0.98
/*
	public MutateRowAction createMutateRow()
	{
		MutateRowAction mutate = new MutateRowAction(task);
		actions.add(mutate);
		return mutate;
	}
*/

	public void execute(HTableInterface table)
	{
		validate();

		try
		{
			List<Row> actions = getBatchActions();
			Object[] results = new Object[actions.size()];
			table.batch(actions, results);

			processExecutionResults(results, table.getName().getNameAsString());
		}
		catch (Exception e)
		{
			throw new BuildException("Failed to execute MUTATE_ROW against " + table.getName().getNameAsString() + " table: " + toString(), e);
		}
	}

	public void executeAsRowMutation(HTableInterface table)
	{
		if (actions.isEmpty())
		{
			throw new BuildException("There are no operations configured for the batch");
		}

		MutateRowAction mutateRowAction = new MutateRowAction(task);

		String rowkey = null;
		for (IAction action : actions)
		{
			if (!(action instanceof DeleteAction) && !(action instanceof PutAction))
			{
				throw new BuildException("Batch contains not only PUT & DELETE operations, " +
						"so it could not be used for MUTATE_ROW operation");
			}

			if (rowkey != null && !rowkey.equals(action.getRowkeyAsString()))
			{
				throw new BuildException("Batch contains operations having different rowkeys, " +
						"so it could not be used for MUTATE_ROW operation");
			}

			rowkey = action.getRowkeyAsString();

			if (action instanceof DeleteAction)
			{
				mutateRowAction.addDelete((DeleteAction)action);
			}
			else
			{
				mutateRowAction.addPut((PutAction)action);
			}
		}

		mutateRowAction.execute(table);
	}

	public void validate()
	{
		if (actions.isEmpty())
		{
			throw new BuildException("No actions specified for a BATCH operation");
		}

		for (IAction action : actions)
		{
			try
			{
				action.validate();
			}
			catch (BuildException e)
			{
				throw new BuildException("Incorrect operation specified for BATCH operation: " + toString());
			}
		}
	}

	public boolean isEmpty()
	{
		return actions.isEmpty();
	}

	@Override
	public String toString()
	{
		StringBuilder builder = new StringBuilder();
		builder.append("BATCH{");

		for (IAction action : actions)
		{
			builder.append(", ").append(action.toString());
		}

		builder.append("}");

		return builder.toString();
	}

	void addAction(IAction action)
	{
		if (action == null)
		{
			throw new IllegalArgumentException("Can't add null action to a batch");
		}

		if (!(action instanceof DeleteAction) && !(action instanceof PutAction) &&
			!(action instanceof GetAction) && !(action instanceof IncrementAction) &&
			!(action instanceof AppendAction) && !(action instanceof MutateRowAction))
		{
			throw new IllegalArgumentException("Can't add action " + action.getClass().getName() + " to a batch");
		}

		actions.add(action);
	}

	private List<Row> getBatchActions()
	{
		List<Row> list = new LinkedList<Row>();
		for (IAction action : actions)
		{
			list.add(action.getAction());
		}

		return list;
	}

	private void processExecutionResults(Object[] results, String table)
	{
		StringBuilder errorSummary = new StringBuilder();

		for (int i = 0; i < results.length; i++)
		{
			Object result = results[i];
			IAction action = actions.get(i);

			if (result == null)
			{
				errorSummary.append("Failed to communicate with remote server to perform operation: ").
						append(action.toString()).
						append(SystemHelper.doubleLineSeparator);
				continue;
			}

			if (result instanceof Throwable)
			{
				errorSummary.append("Failed to perform operation: ").
						append(action.toString()).
						append(SystemHelper.lineSeparator).
						append("Exception occured during execution: ").
						append(ExceptionHelper.stackTraceToString((Throwable)result)).
						append(SystemHelper.doubleLineSeparator);
				continue;
			}

			if (result instanceof Result)
			{
				action.processExecutionResult((Result)result, table);
			}
		}

		if (errorSummary.length() != 0)
		{
			throw new BuildException("BATCH operation failed: " + toString() +
					SystemHelper.lineSeparator + errorSummary.toString());
		}
	}
}
