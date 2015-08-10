package com.anttoolkit.hbase.tasks.data.util;

import java.text.*;
import java.util.*;

import org.apache.hadoop.hbase.*;
import org.apache.hadoop.hbase.util.*;
import org.apache.tools.ant.*;
import org.apache.hadoop.hbase.client.*;

import com.anttoolkit.hbase.tasks.*;

public class CellToGet
{
	private static final String FULL_TEMPLATE = "family={0}, column={1}";
	private static final String SHORT_TEMPLATE = "family={0}";

	private String columnFamily;
	private String column;
	private String type;
	private String property;
	private String reference;
	private String file;
	private Integer version;
	private Long timestamp;

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

	public void setType(String type)
	{
		if (type == null)
		{
			throw new IllegalArgumentException("Cell value type can't be null");
		}

		this.type = type.trim().toUpperCase();
	}

	public void setFile(String file)
	{
		this.file = file;
	}

	public String getFile()
	{
		return file;
	}

	public void setVersion(int version)
	{
		this.version = version;
	}

	public Integer getVersion()
	{
		return version;
	}

	public void setTimestamp(long timestamp)
	{
		this.timestamp = timestamp;
	}

	public Long getTimestamp()
	{
		return timestamp;
	}

	public void setProperty(String property)
	{
		this.property = property;
	}

	public String getProperty()
	{
		return property;
	}

	public void setReference(String reference)
	{
		this.reference = reference;
	}

	public String getReference()
	{
		return reference;
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

		if (file != null && property != null)
		{
			return false;
		}

		if ((file != null || property != null) && column == null)
		{
			return false;
		}

		if (version != null && timestamp != null)
		{
			return false;
		}

		if ((version != null && version < 0) ||
			(timestamp != null && timestamp <= 0))
		{
			return false;
		}

		return columnFamily != null && !columnFamily.trim().isEmpty();
	}

	public void addToGet(Get get)
	{
		if (!isValid())
		{
			throw new BuildException("Can't add incorrect cell configuration to GET operation: " + toString());
		}

		if (column != null && !column.trim().isEmpty())
		{
			get.addColumn(Bytes.toBytes(columnFamily), Bytes.toBytes(column));
		}
		else
		{
			get.addFamily(Bytes.toBytes(columnFamily));
		}
	}

	@Override
	public String toString()
	{
		StringBuilder builder = new StringBuilder();

		builder.append(column != null && !column.trim().isEmpty() ?

				MessageFormat.format(FULL_TEMPLATE,
						columnFamily == null ? "null" : columnFamily,
						column == null ? "null" : column) :

				MessageFormat.format(SHORT_TEMPLATE,
						columnFamily == null ? "null" : columnFamily));

		if (type != null)
		{
			builder.append(", type=").append(type);
		}

		if (property != null)
		{
			builder.append(", property=").append(property);
		}

		if (file != null)
		{
			builder.append(", file=").append(file);
		}

		if (reference != null)
		{
			builder.append(", reference=").append(reference);
		}

		if (version != null)
		{
			builder.append(", version=").append(version.toString());
		}

		if (timestamp != null)
		{
			builder.append(", timestamp=").append(timestamp.toString());
		}

		return builder.toString();
	}

	public CellValueType getType()
			throws IllegalArgumentException
	{
		if (type == null)
		{
			type = file != null || reference != null ? CellValueType.BYTES.toString() : CellValueType.STRING.toString();
		}

		return CellValueType.valueOf(type);
	}

	public boolean processResult(Result result, GenericHBaseTask task)
	{
		if ((property == null || property.trim().isEmpty()) &&
			(file == null || file.trim().isEmpty()) &&
			(reference == null || reference.trim().isEmpty()))
		{
			return false;
		}

		byte[] value = getValue(result);
		if (value == null)
		{
			return false;
		}

		//saving value to Ant property
		if (property != null && !property.trim().isEmpty())
		{
			task.setPropertyThreadSafe(property, getType().decode(value).toString());
		}

		//saving value to file
		if (CellValueType.BYTES.equals(getType()))
		{
			task.saveContentToFile(file, value);
		}
		else
		{
			task.saveContentToFile(file, getType().decode(value).toString());
		}

		//saving value to reference
		if (reference != null && !reference.trim().isEmpty())
		{
			task.setReference(reference, getType().decode(value));
		}

		return true;
	}

	private byte[] getValue(Result result)
	{
		if (columnFamily == null)
		{
			throw new BuildException("Can't get cell value from the result of GET operation, cause column family doesn't specified for the cell");
		}

		if (column == null)
		{
			throw new BuildException("Can't get cell value from the result of GET operation, cause column doesn't specified for the cell");
		}

		// thus no version specified, simply get the latest value
		if (version == null && timestamp == null)
		{
			return result.getValue(Bytes.toBytes(columnFamily), Bytes.toBytes(column));
		}

		// get value specified by version number
		if (version != null)
		{
			List<Cell> cells = result.getColumnCells(Bytes.toBytes(columnFamily), Bytes.toBytes(column));
			return cells.size() <= version ? null : CellUtil.cloneValue(cells.get(version));
		}

		// get value specified by timestamp
		NavigableMap<byte[], NavigableMap<Long, byte[]>> columnsMap = result.getMap().get(Bytes.toBytes(columnFamily));
		if (columnsMap == null)
		{
			return null;
		}

		NavigableMap<Long, byte[]> versionsMap = columnsMap.get(Bytes.toBytes(column));
		if (versionsMap == null)
		{
			return null;
		}

		return versionsMap.get(timestamp);
	}
}
