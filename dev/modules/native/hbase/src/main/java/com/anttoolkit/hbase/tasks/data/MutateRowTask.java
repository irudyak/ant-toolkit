package com.anttoolkit.hbase.tasks.data;

import org.apache.tools.ant.*;

import com.anttoolkit.hbase.tasks.*;
import com.anttoolkit.hbase.tasks.data.util.*;

public class MutateRowTask extends GenericHBaseTask
{
	private String table;
	private MutateRowAction action = new MutateRowAction(this);

	public void setTable(String table)
	{
		this.table = table;
	}

	public void setBatchId(String batchId)
	{
		action.setBatchId(batchId);
	}

	public void setRowkey(String rowkey)
	{
		action.setRowkey(rowkey);
	}

	public DeleteAction createDelete()
	{
		return action.createDelete();
	}

	public PutAction createPut()
	{
		return action.createPut();
	}

	@Override
	protected void doHadoopWork() throws BuildException
	{
		action.execute(getTable(table));
	}

	@Override
	protected void hadoopValidate()
	{
		action.validate();

		if (table == null || table.trim().isEmpty())
		{
			throw new BuildException("Table name should be specified for a MUTATE_ROW operation: " + action.toString());
		}
	}
}
