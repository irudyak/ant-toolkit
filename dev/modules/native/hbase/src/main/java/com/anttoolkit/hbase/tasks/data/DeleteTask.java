package com.anttoolkit.hbase.tasks.data;

import java.io.*;

import org.apache.tools.ant.*;

import com.anttoolkit.hbase.tasks.*;
import com.anttoolkit.hbase.tasks.data.util.*;

public class DeleteTask extends GenericHBaseTask
{
	private String table;
	private DeleteAction action = new DeleteAction(this);

	public void setTable(String table)
	{
		this.table = table;
	}

	public void setRowkey(String rowkey)
	{
		action.setRowkey(rowkey);
	}

	public void setTimestamp(long timestamp)
	{
		action.setTimestamp(timestamp);
	}

	public void addConfiguredCell(CellToDelete cell)
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
			throw new BuildException("Table name should be specified for a DELETE operation: " + action.toString());
		}
	}
}
