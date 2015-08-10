package com.anttoolkit.hbase.tasks.data.util;

import java.io.*;
import java.util.*;

import org.apache.tools.ant.*;
import org.apache.hadoop.hbase.client.*;

import com.anttoolkit.hbase.tasks.*;

public class ColumnExistsAction extends GenericAction<Get>
{
	private String property;
	private List<CellToCheckExists> cells = new LinkedList<CellToCheckExists>();

	public ColumnExistsAction(GenericHBaseTask task)
	{
		super(task);
	}

	public void setProperty(String property)
	{
		this.property = property;
	}

	public void addConfiguredCell(CellToCheckExists cell)
	{
		if (cell == null)
		{
			throw new IllegalArgumentException("Can't add null cell to a EXISTS operation");
		}

		cells.add(cell);
	}

	@Override
	public Get getAction()
	{
		validate();

		Get action = new Get(getRowkey());

		for (CellToCheckExists cell : cells)
		{
			cell.addToGet(action);
		}

		return action;
	}

	@Override
	public void execute(HTableInterface table)
	{
		validate();

		try
		{
			boolean result = table.exists(getAction());

			if (property != null)
			{
				task.setPropertyThreadSafe(property, Boolean.toString(result));
			}
		}
		catch (IOException e)
		{
			throw new BuildException("Failed to execute EXISTS against " + table.getName().getNameAsString() + " table: " + toString(), e);
		}
	}

	@Override
	public void processExecutionResult(Result result, String table)
	{
	}

	@Override
	public String toString()
	{
		StringBuilder builder = new StringBuilder();
		builder.append("EXISTS{rowkey=").append(getRowkeyAsString() == null ? "null" : getRowkeyAsString());

		if (property != null)
		{
			builder.append(", property=").append(property);
		}

		for (CellToCheckExists cell : cells)
		{
			builder.append(", [").append(cell.toString()).append("]");
		}

		builder.append("}");

		return builder.toString();
	}

	@Override
	public void validate()
	{
		super.validate();

		if (!checkCellsValid())
		{
			throw new BuildException("Incorrect cells specification for a EXISTS operation: " + toString());
		}
	}

	private boolean checkCellsValid()
	{
		for (CellToCheckExists cell : cells)
		{
			if (!cell.isValid())
			{
				return false;
			}
		}

		return true;
	}
}
