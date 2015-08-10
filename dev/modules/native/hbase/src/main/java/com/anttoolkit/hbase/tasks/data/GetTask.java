package com.anttoolkit.hbase.tasks.data;

import org.apache.tools.ant.*;

import com.anttoolkit.hbase.tasks.*;
import com.anttoolkit.hbase.tasks.data.util.*;

public class GetTask extends GenericHBaseTask
{
	private String table;
	private GetAction action = new GetAction(this);

	public void setTable(String table)
	{
		this.table = table;
	}

	public void setReference(String ref)
	{
		action.setReference(ref);
	}

	public void setRowkey(String rowkey)
	{
		action.setRowkey(rowkey);
	}

	public void setCacheBlocks(boolean cache)
	{
		action.setCacheBlocks(cache);
	}

	public void setMaxVersions(int maxVersions)
	{
		action.setMaxVersions(maxVersions);
	}

	public void setMinTimestamp(long timestamp)
	{
		action.setMinTimestamp(timestamp);
	}

	public void setMaxTimestamp(long timestamp)
	{
		action.setMaxTimestamp(timestamp);
	}

	public void setTimestamp(long timestamp)
	{
		action.setTimestamp(timestamp);
	}

	public void addConfiguredCell(CellToGet cell)
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
			throw new BuildException("Table name should be specified for a GET operation: " + action.toString());
		}
	}
}
