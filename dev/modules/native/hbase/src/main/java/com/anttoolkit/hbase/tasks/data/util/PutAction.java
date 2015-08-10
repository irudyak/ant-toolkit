package com.anttoolkit.hbase.tasks.data.util;

import java.io.*;
import java.util.*;

import org.apache.tools.ant.*;
import org.apache.hadoop.hbase.client.*;

import com.anttoolkit.hbase.tasks.*;

public class PutAction extends GenericAction<Put>
{
	private List<CellToPut> cells = new LinkedList<CellToPut>();

	public PutAction(GenericHBaseTask task)
	{
		super(task);
	}

	public void addConfiguredCell(CellToPut cell)
	{
		if (cell == null)
		{
			throw new IllegalArgumentException("Can't add null cell to a PUT operation");
		}

		cells.add(cell.init(task));
	}

	@Override
	public Put getAction()
	{
		validate();

		Put action = new Put(getRowkey());

		for (CellToPut cell : cells)
		{
			cell.addToPut(action);
		}

		return action;
	}

	@Override
	public void execute(HTableInterface table)
	{
		validate();

		try
		{
			table.put(getAction());
		}
		catch (IOException e)
		{
			throw new BuildException("Failed to execute PUT against " + table.getName().getNameAsString() + " table: " + toString(), e);
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

		if (cells.isEmpty())
		{
			throw new BuildException("No cells specified for a PUT operation: " + toString());
		}

		if (!checkCellsValid())
		{
			throw new BuildException("Incorrect cells specification for a PUT operation: " + toString());
		}
	}

	@Override
	public String toString()
	{
		StringBuilder builder = new StringBuilder();
		builder.append("PUT{rowkey=").append(getRowkeyAsString() == null ? "null" : getRowkeyAsString());

		for (CellToPut cell : cells)
		{
			builder.append(", [").append(cell.toString()).append("]");
		}

		builder.append("}");

		return builder.toString();
	}

	private boolean checkCellsValid()
	{
		for (CellToPut cell : cells)
		{
			if (!cell.isValid())
			{
				return false;
			}
		}

		return true;
	}
}
