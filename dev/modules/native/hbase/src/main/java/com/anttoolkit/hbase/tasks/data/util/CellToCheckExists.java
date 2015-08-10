package com.anttoolkit.hbase.tasks.data.util;

import java.text.*;

import org.apache.hadoop.hbase.client.*;
import org.apache.hadoop.hbase.util.*;
import org.apache.tools.ant.*;

public class CellToCheckExists
{
	private static final String TEMPLATE = "family={0}, column={1}";

	private String columnFamily;
	private String column;

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

	public boolean isValid()
	{
		return columnFamily != null && !columnFamily.trim().isEmpty() &&
				column != null && !column.trim().isEmpty();
	}

	public void addToGet(Get get)
	{
		if (!isValid())
		{
			throw new BuildException("Can't add incorrect cell configuration to EXISTS operation: " + toString());
		}

		get.addColumn(Bytes.toBytes(columnFamily), Bytes.toBytes(column));
	}

	@Override
	public String toString()
	{
		return MessageFormat.format(TEMPLATE,
				columnFamily == null ? "null" : columnFamily,
				column == null ? "null" : column);
	}
}
