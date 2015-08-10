package com.anttoolkit.hbase.tasks.data.util;

import java.io.*;
import java.util.*;

import org.apache.tools.ant.*;
import org.apache.hadoop.hbase.client.*;

import com.anttoolkit.hbase.tasks.*;

public class DeleteAction extends GenericAction<Delete>
{
	private long timestamp = -1;
	private List<CellToDelete> cells = new LinkedList<CellToDelete>();

	public DeleteAction(GenericHBaseTask task)
	{
		super(task);
	}

	public void setTimestamp(long timestamp)
	{
		this.timestamp = timestamp;
	}

	public void addConfiguredCell(CellToDelete cell)
	{
		if (cell == null)
		{
			throw new IllegalArgumentException("Can't add null cell to a DELETE operation");
		}

		cells.add(cell);
	}

	@Override
	public Delete getAction()
	{
		validate();

		Delete action = timestamp != -1 ?
				new Delete(getRowkey(), timestamp) :
				new Delete(getRowkey());

		for (CellToDelete cell : cells)
		{
			cell.addToDelete(action);
		}

		return action;
	}

	@Override
	public void execute(HTableInterface table)
	{
		validate();

		try
		{
			table.delete(getAction());
		}
		catch (IOException e)
		{
			throw new BuildException("Failed to execute DELETE against " + table.getName().getNameAsString() + " table: " + toString(), e);
		}
	}

	@Override
	public void processExecutionResult(Result result, String table)
	{
	}

	@Override
	public void validate()
	{
		super.validate();

		if (!checkCellsValid())
		{
			throw new BuildException("Incorrect cells specification for a DELETE operation: " + toString());
		}
	}

	@Override
	public String toString()
	{
		StringBuilder builder = new StringBuilder();
		builder.append("DELETE{rowkey=").append(getRowkeyAsString() == null ? "null" : getRowkeyAsString());

		if (timestamp != -1)
		{
			builder.append(", ts=").append(timestamp);
		}

		for (CellToDelete cell : cells)
		{
			builder.append(", [").append(cell.toString()).append("]");
		}

		builder.append("}");

		return builder.toString();
	}

	private boolean checkCellsValid()
	{
		for (CellToDelete cell : cells)
		{
			if (!cell.isValid())
			{
				return false;
			}
		}

		return true;
	}

}
