package com.anttoolkit.hbase.tasks.data.util;

import java.text.*;

import org.apache.hadoop.hbase.client.*;
import org.apache.hadoop.hbase.util.*;
import org.apache.tools.ant.*;

public class CellToDelete
{
	private static final String FULL_TEMPLATE = "family={0}, column={1}, ts={2}";
	private static final String SHORT_TEMPLATE1 = "family={0}, column={1}";
	private static final String SHORT_TEMPLATE2 = "family={0}, ts={1}";
	private static final String SHORT_TEMPLATE3 = "family={0}";

	private String columnFamily;
	private String column;
	private long timestamp = -1;
	private boolean allVersions = false;

	public void setColumnFamily(String columnFamily)
	{
		this.columnFamily = columnFamily;
	}

	public void setColumn(String column)
	{
		this.column = column;
	}

	public void setTimestamp(long timestamp)
	{
		this.timestamp = timestamp;
	}

	public void setAllVersions(boolean allVersions)
	{
		this.allVersions = allVersions;
	}

	public boolean isValid()
	{
		return columnFamily != null && !columnFamily.trim().isEmpty() &&
				(column == null || !column.trim().isEmpty());
	}

	public void addToDelete(Delete delete)
	{
		if (!isValid())
		{
			throw new BuildException("Can't add incorrect cell configuration to DELETE operation: " + toString());
		}

		if (column != null && timestamp != -1)
		{
			if (!allVersions)
			{
				delete.deleteColumn(Bytes.toBytes(columnFamily), Bytes.toBytes(column), timestamp);
			}
			else
			{
				delete.deleteColumns(Bytes.toBytes(columnFamily), Bytes.toBytes(column), timestamp);
			}
			return;
		}

		if (column != null)
		{
			if (!allVersions)
			{
				delete.deleteColumn(Bytes.toBytes(columnFamily), Bytes.toBytes(column));
			}
			else
			{
				delete.deleteColumns(Bytes.toBytes(columnFamily), Bytes.toBytes(column));
			}
			return;
		}

		if (timestamp != -1)
		{
			delete.deleteFamily(Bytes.toBytes(columnFamily), timestamp);
			return;
		}

		delete.deleteFamily(Bytes.toBytes(columnFamily));
	}

	@Override
	public String toString()
	{
		if (column != null && timestamp != -1)
		{
			return MessageFormat.format(FULL_TEMPLATE, columnFamily == null ? "null" : columnFamily, column, timestamp);
		}

		if (column != null)
		{
			return MessageFormat.format(SHORT_TEMPLATE1, columnFamily == null ? "null" : columnFamily, column);
		}

		if (timestamp != -1)
		{
			return MessageFormat.format(SHORT_TEMPLATE2, columnFamily == null ? "null" : columnFamily, timestamp);
		}

		return MessageFormat.format(SHORT_TEMPLATE3, columnFamily == null ? "null" : columnFamily);
	}
}
