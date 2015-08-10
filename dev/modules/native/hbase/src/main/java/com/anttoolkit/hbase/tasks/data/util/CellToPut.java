package com.anttoolkit.hbase.tasks.data.util;

import java.text.*;

import org.apache.hadoop.hbase.client.*;
import org.apache.hadoop.hbase.util.*;
import org.apache.tools.ant.*;

import com.anttoolkit.hbase.tasks.*;

public class CellToPut
{
	private static final String FULL_TEMPLATE = "family={0}, column={1}, ts={2}, value={3}{4}";
	private static final String SHORT_TEMPLATE = "family={0}, column={1}, value={2}{3}";

	private String columnFamily;
	private String column;
	private long timestamp = -1;
	private String value;
	private String reference;
	private String file;
	private String type;
	private GenericHBaseTask task;

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

	public void setValue(String value)
	{
		this.value = value;
	}

	public void setReference(String reference)
	{
		this.reference = reference;
	}

	public void setType(String type)
	{
		if (type == null)
		{
			throw new IllegalArgumentException("Cell value type can't be null");
		}

		this.type = type.trim().toUpperCase();
	}

	public void addText(String value)
	{
		this.value = value;
	}

	public void setFile(String file)
	{
		this.file = file;
	}

	public boolean isValid()
	{
		try
		{
			getType();
		}
		catch (IllegalArgumentException e)
		{
			return false;
		}

		if (file != null)
		{
			if (!CellValueType.STRING.equals(getType()) &&
				!CellValueType.BYTES.equals(getType()))
			{
				return false;
			}

			if (!task.fileExists(file))
			{
				return false;
			}
		}

		if (reference != null && !task.checkReferenceExists(reference))
		{
			return false;
		}

		return columnFamily != null && !columnFamily.trim().isEmpty() &&
				column != null && !column.trim().isEmpty() && value != null;
	}

	public void addToPut(Put put)
	{
		if (!isValid())
		{
			throw new BuildException("Can't add incorrect cell configuration to PUT operation: " + toString());
		}

		if (timestamp != -1)
		{
			put.addImmutable(Bytes.toBytes(columnFamily), Bytes.toBytes(column), timestamp, getRawValue());
		}
		else
		{
			put.addImmutable(Bytes.toBytes(columnFamily), Bytes.toBytes(column), getRawValue());
		}
	}

	@Override
	public String toString()
	{
		return timestamp != -1 ?

				MessageFormat.format(FULL_TEMPLATE,
						columnFamily == null ? "null" : columnFamily,
						column == null ? "null" : column,
						timestamp,
						type == null ? "(null)" : type.equals(CellValueType.STRING.toString()) ? "" : "(" + type + ")",
						value == null ? "null" : value) :

				MessageFormat.format(SHORT_TEMPLATE,
						columnFamily == null ? "null" : columnFamily,
						column == null ? "null" : column,
						type == null ? "(null)" : type.equals(CellValueType.STRING.toString()) ? "" : "(" + type + ")",
						value == null ? "null" : value);
	}

	CellToPut init(GenericHBaseTask task)
	{
		this.task = task;

		if (file != null)
		{
			value = "(FILE)" + task.getFileFullPath(file);
		}
		else if (reference != null)
		{
			value = "(REFERENCE)" + reference;
		}

		if (type == null)
		{
			type = file != null ?
					CellValueType.BYTES.toString() :
					reference != null ?
							CellValueType.detectEncodingType(task.getReference(reference)).toString() :
							CellValueType.STRING.toString();
		}

		return this;
	}

	private CellValueType getType()
			throws IllegalArgumentException
	{
		if (reference != null)
		{
			return CellValueType.detectEncodingType(task.getReference(reference));
		}

		if (type == null)
		{
			type = file != null ? CellValueType.BYTES.toString() : CellValueType.STRING.toString();
		}

		return CellValueType.valueOf(type);
	}

	private byte[] getRawValue()
	{
		if (file != null && !file.trim().isEmpty())
		{
			return CellValueType.BYTES.equals(getType()) ?
					task.loadFileContentRaw(file) :
					Bytes.toBytes(task.loadFileContent(file));
		}

		return reference == null ?
				getType().encode(value) :
				CellValueType.encodeObject(task.getReference(reference));
	}
}
