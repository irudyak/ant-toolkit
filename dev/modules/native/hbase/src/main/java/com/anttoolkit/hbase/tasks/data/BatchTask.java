package com.anttoolkit.hbase.tasks.data;

import org.apache.tools.ant.*;

import com.anttoolkit.hbase.tasks.*;
import com.anttoolkit.hbase.tasks.data.util.*;

public class BatchTask extends GenericHBaseTask
{
	private String batchId;
	private BatchAction action = new BatchAction(this);

	public void setBatchId(String batchId)
	{
		this.batchId = batchId;
	}

	public DeleteAction createDelete()
	{
		return action.createDelete();
	}

	public PutAction createPut()
	{
		return action.createPut();
	}

	public GetAction createGet()
	{
		return action.createGet();
	}

	public IncrementAction createIncrement()
	{
		return action.createIncrement();
	}

	public AppendAction createAppend()
	{
		return action.createAppend();
	}

// Row mutation operations are not supported for batch execution in HBase release 0.98
/*

	public MutateRowAction createMutateRow()
	{
		return action.createMutateRow();
	}
*/

	@Override
	protected void doHadoopWork() throws BuildException
	{
		BatchManager.init(batchId, action);
	}

	@Override
	protected void hadoopValidate()
	{
		if (batchId == null || batchId.trim().isEmpty())
		{
			throw new BuildException("Batch ID should be specified");
		}

		if (!action.isEmpty())
		{
			action.validate();
		}
	}
}
