package com.anttoolkit.hbase.tasks.data;

import org.apache.tools.ant.*;

import com.anttoolkit.hbase.tasks.*;
import com.anttoolkit.hbase.tasks.data.util.*;

public class IncrementTask extends GenericHBaseTask
{
	private String table;
	private IncrementAction action = new IncrementAction(this);

	public void setTable(String table)
	{
		this.table = table;
	}

	public void setRowkey(String rowkey)
	{
		action.setRowkey(rowkey);
	}

	public void addConfiguredCell(CellToIncrement cell)
	{
		action.addConfiguredCell(cell);
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
			throw new BuildException("Table name should be specified for an INCREMENT operation: " + action.toString());
		}
	}
}
