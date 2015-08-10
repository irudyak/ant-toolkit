package com.anttoolkit.hbase.tasks.data;

import java.io.*;

import org.apache.hadoop.hbase.client.*;
import org.apache.tools.ant.*;

import com.anttoolkit.hbase.tasks.*;
import com.anttoolkit.hbase.tasks.data.util.*;

public class ExecuteBatchTask extends GenericHBaseTask
{
	private String table;
	private String batchId;
	private Long writeBufferSize;
	private BatchAction action = new BatchAction(this);

	public void setBatchId(String batchId)
	{
		this.batchId = batchId;
	}

	public void setTable(String table)
	{
		this.table = table;
	}

	public void setWriteBufferSize(long writeBufferSize)
	{
		this.writeBufferSize = writeBufferSize;
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
		HTableInterface htable = getTable(table);
		if (writeBufferSize != null)
		{
			try
			{
				htable.setWriteBufferSize(writeBufferSize);
			}
			catch (IOException e)
			{
				throw new BuildException("Failed to set writeBufferSize for BATCH operation: " + toString(), e);
			}
		}

		if (batchId == null)
		{
			action.execute(htable);
		}
		else
		{
			BatchManager.execute(batchId, htable);
		}
	}

	@Override
	protected void hadoopValidate()
	{
		if ((batchId == null && action.isEmpty()) ||
			(batchId != null && !action.isEmpty()))
		{
			throw new BuildException("Either batch ID or actions should be specified inside BATCH operation: " + toString());
		}

		if (batchId == null)
		{
			action.validate();
		}

		if (table == null || table.trim().isEmpty())
		{
			throw new BuildException("Table name should be specified for a BATCH operation: " + toString());
		}

		if (writeBufferSize != null && writeBufferSize < 1)
		{
			throw new BuildException("Incorrect writeBufferSize specified for BATCH operation: " + toString());
		}
	}
}
