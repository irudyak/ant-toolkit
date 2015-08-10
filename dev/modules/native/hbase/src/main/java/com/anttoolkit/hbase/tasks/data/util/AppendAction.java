package com.anttoolkit.hbase.tasks.data.util;

import java.io.*;
import java.util.*;

import org.apache.tools.ant.*;
import org.apache.hadoop.hbase.client.*;

import com.anttoolkit.hbase.tasks.*;

public class AppendAction extends GenericAction<Append>
{
	private List<CellToAppend> cells = new LinkedList<CellToAppend>();

	public AppendAction(GenericHBaseTask task)
	{
		super(task);
	}

	public void addConfiguredCell(CellToAppend cell)
	{
		if (cell == null)
		{
			throw new IllegalArgumentException("Can't add null cell to an APPEND operation");
		}

		cells.add(cell.init(task));
	}

	@Override
	public Append getAction()
	{
		validate();

		Append action = new Append(getRowkey());

		for (CellToAppend cell : cells)
		{
			cell.addToAppend(action);
		}

		return action;
	}

	@Override
	public void execute(HTableInterface table)
	{
		validate();

		try
		{
			table.append(getAction());
		}
		catch (IOException e)
		{
			throw new BuildException("Failed to execute APPEND against " + table.getName().getNameAsString() + " table: " + toString(), e);
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
			throw new BuildException("No cells specified for an APPEND operation: " + toString());
		}

		if (!checkCellsValid())
		{
			throw new BuildException("Incorrect cells specification for an APPEND operation: " + toString());
		}
	}

	@Override
	public String toString()
	{
		StringBuilder builder = new StringBuilder();
		builder.append("APPEND{rowkey=").append(getRowkeyAsString() == null ? "null" : getRowkeyAsString());

		for (CellToAppend cell : cells)
		{
			builder.append(", [").append(cell.toString()).append("]");
		}

		builder.append("}");

		return builder.toString();
	}

	private boolean checkCellsValid()
	{
		for (CellToAppend cell : cells)
		{
			if (!cell.isValid())
			{
				return false;
			}
		}

		return true;
	}
}
