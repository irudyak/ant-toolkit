package com.anttoolkit.hbase.tasks.data.util;

import java.io.*;
import java.util.*;

import com.anttoolkit.hbase.tasks.*;
import org.apache.tools.ant.*;
import org.apache.hadoop.hbase.client.*;

public class MutateRowAction extends GenericAction<RowMutations>
{
	private String batchId;
	private List<IAction> actions = new LinkedList<IAction>();

	public MutateRowAction(GenericHBaseTask task)
	{
		super(task);
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

	public void setBatchId(String batchId)
	{
		this.batchId = batchId;
	}

	@Override
	public RowMutations getAction()
	{
		validate();

		RowMutations mutations = new RowMutations(getRowkey());

		try
		{
			for (IAction action : actions)
			{
				Row act = action.getAction();
				if (act instanceof Delete)
				{
					mutations.add((Delete)act);
				}
				else if (act instanceof Put)
				{
					mutations.add((Put)act);
				}
			}
		}
		catch (IOException e)
		{
			throw new BuildException("Error occured while preparing MUTATE_ROW operation: " + toString(), e);
		}

		return mutations;
	}

	@Override
	public void execute(HTableInterface table)
	{
		validate();

		if (batchId == null)
		{
			try
			{
				table.mutateRow(getAction());
			}
			catch (IOException e)
			{
				throw new BuildException("Failed to execute MUTATE_ROW against " + table.getName().getNameAsString() + " table: " + toString(), e);
			}
		}
		else
		{
			try
			{
				BatchManager.executeAsRowMutation(batchId, table);
			}
			catch (Throwable e)
			{
				throw new BuildException("Failed to execute MUTATE_ROW against " + table.getName().getNameAsString() + " table, using batch '" + batchId + "' :" + toString(), e);
			}
		}
	}

	@Override
	public void processExecutionResult(Result result, String table)
	{
	}

	@Override
	public void validate()
	{
		if ((batchId == null && actions.isEmpty()) &&
			(batchId != null && !actions.isEmpty()))
		{
			throw new BuildException("Either batchId or actions should be specified for a MUTATE_ROW operation: " + toString());
		}

		if (batchId != null)
		{
			return;
		}

		checkAllActionsHaveRowkey();

		String rowkey = this.getRowkeyAsString();
		for (IAction action : actions)
		{
			if (!rowkey.equals(action.getRowkeyAsString()))
			{
				throw new BuildException("Several operations of a MUTATE_ROW operation have different rowkeys: " + toString());
			}

			try
			{
				action.validate();
			}
			catch (BuildException e)
			{
				throw new BuildException("Incorrect operation specified for MUTATE_ROW operation: " + toString());
			}
		}
	}

	@Override
	public String toString()
	{
		StringBuilder builder = new StringBuilder();
		builder.append("MUTATE_ROW{");

		if (batchId != null)
		{
			builder.append("batchId=").append(batchId);
		}

		for (IAction action : actions)
		{
			builder.append(", ").append(action.toString());
		}

		builder.append("}");

		return builder.toString();
	}

	void addDelete(DeleteAction delete)
	{
		actions.add(delete);
	}

	void addPut(PutAction put)
	{
		actions.add(put);
	}

	private void checkAllActionsHaveRowkey()
	{
		if (getRowkey() == null)
		{
			return;
		}

		for (IAction action : actions)
		{
			if (action.getRowkey() == null)
			{
				action.setRowkeyRaw(getRowkey());
				action.setRowkeyType(getRowkeyType().toString());
			}
		}
	}
}
