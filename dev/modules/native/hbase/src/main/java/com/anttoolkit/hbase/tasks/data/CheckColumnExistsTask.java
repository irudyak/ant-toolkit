package com.anttoolkit.hbase.tasks.data;

import org.apache.tools.ant.*;

import com.anttoolkit.hbase.tasks.*;
import com.anttoolkit.hbase.tasks.data.util.*;

public class CheckColumnExistsTask extends GenericHBaseTask
{
	private String table;
	private ColumnExistsAction action = new ColumnExistsAction(this);

	public void setTable(String table)
	{
		this.table = table;
	}

	public void setRowkey(String rowkey)
	{
		action.setRowkey(rowkey);
	}

	public void setProperty(String property)
	{
		action.setProperty(property);
	}

	public void addConfiguredCell(CellToCheckExists cell)
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
			throw new BuildException("Table name should be specified for an EXISTS operation: " + action.toString());
		}
	}
}
