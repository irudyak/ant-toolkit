package com.anttoolkit.hbase.tasks.data.util;

import java.io.*;
import java.util.*;

import org.apache.tools.ant.*;
import org.apache.hadoop.hbase.client.*;
import org.apache.hadoop.hbase.util.*;

import com.anttoolkit.hbase.tasks.*;

public class IncrementAction extends GenericAction<Increment>
{
	private List<CellToIncrement> cells = new LinkedList<CellToIncrement>();

	public IncrementAction(GenericHBaseTask task)
	{
		super(task);
	}

	public void addConfiguredCell(CellToIncrement cell)
	{
		if (cell == null)
		{
			throw new IllegalArgumentException("Can't add null cell to an INCREMENT operation");
		}

		cells.add(cell);
	}

	@Override
	public Increment getAction()
	{
		validate();

		Increment action = new Increment(getRowkey());

		for (CellToIncrement cell : cells)
		{
			cell.addToIncrement(action);
		}

		return action;
	}

	@Override
	public void execute(HTableInterface table)
	{
		validate();

		try
		{
			if (cells.size() == 1)
			{
				CellToIncrement cell = cells.get(0);

				long result = table.incrementColumnValue(getRowkey(),
						Bytes.toBytes(cell.getColumnFamily()),
						Bytes.toBytes(cell.getColumn()),
						cell.getAmount());

				if (cell.getProperty() != null)
				{
					task.setPropertyThreadSafe(cell.getProperty(), Long.toString(result));
				}

				return;
			}

			processExecutionResult(table.increment(getAction()), table.getName().getNameAsString());
		}
		catch (IOException e)
		{
			throw new BuildException("Failed to execute INCREMENT against " + table.getName().getNameAsString() + " table: " + toString(), e);
		}
	}

	@Override
	public void processExecutionResult(Result result, String table)
	{
		NavigableMap<byte[], NavigableMap<byte[], byte[]>> map = result.getNoVersionMap();

		for (CellToIncrement cell : cells)
		{
			if (cell.getProperty() == null)
			{
				continue;
			}

			long value = Bytes.toLong(map.get(Bytes.toBytes(cell.getColumnFamily())).get(Bytes.toBytes(cell.getColumn())));

			task.setPropertyThreadSafe(cell.getProperty(), Long.toString(value));
		}
	}

	@Override
	public void validate()
	{
		super.validate();

		if (cells.isEmpty())
		{
			throw new BuildException("No cells specified for an INCREMENT operation: " + toString());
		}

		if (!checkCellsValid())
		{
			throw new BuildException("Incorrect cells specification for an INCREMENT operation: " + toString());
		}
	}

	@Override
	public String toString()
	{
		StringBuilder builder = new StringBuilder();
		builder.append("INCREMENT{rowkey=").append(getRowkeyAsString() == null ? "null" : getRowkeyAsString());

		for (CellToIncrement cell : cells)
		{
			builder.append(", [").append(cell.toString()).append("]");
		}

		builder.append("}");

		return builder.toString();
	}

	private boolean checkCellsValid()
	{
		for (CellToIncrement cell : cells)
		{
			if (!cell.isValid())
			{
				return false;
			}
		}

		return true;
	}
}
