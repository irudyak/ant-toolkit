package com.anttoolkit.hbase.tasks.data.util;

import java.text.*;

import org.apache.tools.ant.*;
import org.apache.hadoop.hbase.client.*;
import org.apache.hadoop.hbase.util.*;

public class CellToIncrement
{
	private static final String TEMPLATE = "family={0}, column={1}, amount={2}";

	private String columnFamily;
	private String column;
	private String amount;
	private String property;

	public void setColumnFamily(String columnFamily)
	{
		this.columnFamily = columnFamily;
	}

	public String getColumnFamily()
	{
		return columnFamily;
	}

	public void setColumn(String column)
	{
		this.column = column;
	}

	public String getColumn()
	{
		return column;
	}

	public void setAmount(String amount)
	{
		try
		{
			Long.parseLong(amount);
			this.amount = amount;
		}
		catch (NumberFormatException e)
		{
			throw new BuildException("Incorrect amount " + amount + " specified for an INCREMENT operation", e);
		}
	}

	public long getAmount()
	{
		return Long.parseLong(amount);
	}

	public void addText(String amount)
	{
		setAmount(amount);
	}

	public void setProperty(String property)
	{
		this.property = property;
	}

	public String getProperty()
	{
		return property;
	}

	public boolean isValid()
	{
		if (amount == null)
		{
			return false;
		}

		try
		{
			Long.parseLong(amount);
		}
		catch (NumberFormatException e)
		{
			return false;
		}

		return columnFamily != null && !columnFamily.trim().isEmpty() &&
				column != null && !column.trim().isEmpty();
	}

	public void addToIncrement(Increment increment)
	{
		if (!isValid())
		{
			throw new BuildException("Can't add incorrect cell configuration to INCREMENT operation: " + toString());
		}

		increment.addColumn(Bytes.toBytes(columnFamily), Bytes.toBytes(column), Long.parseLong(amount));
	}

	@Override
	public String toString()
	{
		return MessageFormat.format(TEMPLATE,
				columnFamily == null ? "null" : columnFamily,
				column == null ? "null" : column,
				amount == null ? "null" : amount);
	}
}
