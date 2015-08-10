package com.anttoolkit.hbase.tasks.data;

import java.io.*;
import java.text.*;

import org.apache.hadoop.hbase.util.*;
import org.apache.tools.ant.*;

import com.anttoolkit.hbase.tasks.*;
import com.anttoolkit.hbase.tasks.data.util.*;

public class CheckAndDeleteTask extends GenericHBaseTask
{
	private static final String TEMPLATE = "CHECK_AND_DELETE'{'rowkey={0}, family={1}, column={2}, value={3}, {4}'}'";

	private String table;
	private String rowkey;
	private String columnFamily;
	private String column;
	private String value;
	private DeleteAction delete = new DeleteAction(this);
	private String property;

	public void setTable(String table)
	{
		this.table = table;
	}

	public void setRowkey(String rowkey)
	{
		this.rowkey = rowkey;
		delete.setRowkey(rowkey);
	}

	public void setColumnFamily(String columnFamily)
	{
		this.columnFamily = columnFamily;
	}

	public void setColumn(String column)
	{
		this.column = column;
	}

	public void setValue(String value)
	{
		this.value = value;
	}

	public void setProperty(String property)
	{
		this.property = property;
	}

	public void addConfiguredCell(CellToDelete cell)
	{
		delete.addConfiguredCell(cell);
	}

	@Override
	protected void doHadoopWork() throws BuildException
	{
		try
		{
			boolean result = getTable(table).checkAndDelete(Bytes.toBytes(rowkey), Bytes.toBytes(columnFamily),
					Bytes.toBytes(column), Bytes.toBytes(value), delete.getAction());

			if (property != null)
			{
				setPropertyThreadSafe(property, Boolean.toString(result));
			}
		}
		catch (IOException e)
		{
			throw new BuildException("Failed to execute CHECK_AND_PUT against " + table + " table: " + this.toString(), e);
		}
	}

	@Override
	protected void hadoopValidate()
	{
		if (table == null || table.trim().isEmpty())
		{
			throw new BuildException("Table name should be specified for a CHECK_AND_DELETE operation");
		}

		if (rowkey == null || rowkey.trim().isEmpty())
		{
			throw new BuildException("Rowkey should be specified for a CHECK_AND_DELETE operation");
		}

		if (columnFamily == null || columnFamily.trim().isEmpty())
		{
			throw new BuildException("Column family should be specified for a CHECK_AND_DELETE operation");
		}

		if (column == null || column.trim().isEmpty())
		{
			throw new BuildException("Column should be specified for a CHECK_AND_DELETE operation");
		}

		if (value == null)
		{
			throw new BuildException("Value should be specified for a CHECK_AND_DELETE operation");
		}

		if (delete == null)
		{
			throw new BuildException("DELETE operation should be specified for a CHECK_AND_DELETE operation");
		}

		delete.validate();
	}

	public String toString()
	{
		return MessageFormat.format(TEMPLATE,
				rowkey == null ? "null" : rowkey,
				columnFamily == null ? "null" : columnFamily,
				column == null ? "null" : column,
				value == null ? "null" : value,
				delete == null ? "null" : delete);
	}
}
