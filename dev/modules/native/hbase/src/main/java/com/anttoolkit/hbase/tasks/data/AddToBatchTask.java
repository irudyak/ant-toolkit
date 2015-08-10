package com.anttoolkit.hbase.tasks.data;

import java.util.*;

import org.apache.tools.ant.*;

import com.anttoolkit.hbase.tasks.*;
import com.anttoolkit.hbase.tasks.data.util.*;

public class AddToBatchTask extends GenericHBaseTask
{
	private String batchId;
	private List<IAction> actions = new LinkedList<IAction>();

	public void setBatchId(String batchId)
	{
		this.batchId = batchId;
	}

	public DeleteAction createDelete()
	{
		DeleteAction delete = new DeleteAction(this);
		actions.add(delete);
		return delete;
	}

	public PutAction createPut()
	{
		PutAction put = new PutAction(this);
		actions.add(put);
		return put;
	}

	public GetAction createGet()
	{
		GetAction get = new GetAction(this);
		actions.add(get);
		return get;
	}

	public IncrementAction createIncrement()
	{
		IncrementAction increment = new IncrementAction(this);
		actions.add(increment);
		return increment;
	}

	public AppendAction createAppend()
	{
		AppendAction append = new AppendAction(this);
		actions.add(append);
		return append;
	}

// Row mutation operations are not supported for batch execution in HBase release 0.98
/*
	public MutateRowAction createMutateRow()
	{
		MutateRowAction mutate = new MutateRowAction(this);
		actions.add(mutate);
		return mutate;
	}
*/

	@Override
	protected void doHadoopWork() throws BuildException
	{
		for (IAction action : actions)
		{
			BatchManager.addAction(batchId, action);
		}
	}

	@Override
	protected void hadoopValidate()
	{
		if (batchId == null || batchId.trim().isEmpty())
		{
			throw new BuildException("Batch ID should be specified");
		}

		if (!BatchManager.exists(batchId))
		{
			throw new BuildException("There was no batch '" + batchId + "' previously initialized");
		}

		if (actions.isEmpty())
		{
			throw new BuildException("No actions specified to add to batch " + batchId);
		}
	}
}
