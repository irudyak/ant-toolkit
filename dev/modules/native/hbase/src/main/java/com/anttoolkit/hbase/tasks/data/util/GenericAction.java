package com.anttoolkit.hbase.tasks.data.util;

import org.apache.hadoop.hbase.client.*;
import org.apache.hadoop.hbase.util.*;
import org.apache.tools.ant.*;

import com.anttoolkit.hbase.tasks.*;

public abstract class GenericAction<A extends Row> implements IAction<A>
{
	private String rowkey;
	private byte[] rowkeyRaw;
	private String rowkeyRef;
	private String rowkeyType;
	protected final GenericHBaseTask task;

	public GenericAction(GenericHBaseTask task)
	{
		this.task = task;
	}

	@Override
	public void setRowkey(String rowkey)
	{
		this.rowkey = rowkey;
	}

	public void setRowkeyRaw(byte[] rowkeyRaw)
	{
		this.rowkeyRaw = rowkeyRaw;
	}

	@Override
	public void setRowkeyRef(String rowkeyRef)
	{
		this.rowkeyRef = rowkeyRef;
	}

	@Override
	public void setRowkeyType(String type)
	{
		this.rowkeyType = type;
	}

	@Override
	public byte[] getRowkey()
	{
		if (rowkeyRaw != null)
		{
			return rowkeyRaw;
		}

		validate();

		return rowkeyRef == null ?
				getRowkeyType().encode(rowkey) :
				CellValueType.encodeObject(task.getReference(rowkeyRef));
	}

	@Override
	public String getRowkeyAsString()
	{
		if (rowkeyRef == null && rowkey == null)
		{
			return null;
		}

		if (rowkeyRef == null)
		{
			return rowkey;
		}

		if (!task.checkReferenceExists(rowkeyRef) || task.getReference(rowkeyRef) == null)
		{
			return null;
		}

		return !CellValueType.BYTES.equals(getRowkeyType()) ?
				getRowkeyType().decode(getRowkey()).toString() :
				Bytes.toStringBinary(getRowkey());
	}

	public void validate()
	{
		if ((rowkey == null && rowkeyRef == null) ||
			(rowkey != null && rowkeyRef != null))
		{
			throw new BuildException("Either rowkey or rowkeyRef should be specified");
		}

		if (rowkeyRef != null && !task.checkReferenceExists(rowkeyRef))
		{
			throw new BuildException("Specified rowkeyRef '" +rowkeyRef + "' doesn't exist");
		}

		if (rowkeyRef != null && task.getReference(rowkeyRef) == null)
		{
			throw new BuildException("Specified rowkeyRef '" +rowkeyRef + "' contains null object");
		}

		try
		{
			getRowkeyType();
		}
		catch (Throwable e)
		{
			throw new BuildException("Incorrect rowkey type specified", e);
		}
	}

	public CellValueType getRowkeyType()
			throws IllegalArgumentException
	{
		if (rowkeyType == null)
		{
			if (rowkeyRaw != null)
			{
				rowkeyType = CellValueType.BYTES.toString();
			}
			else if (rowkeyRef != null)
			{
				rowkeyType = CellValueType.detectEncodingType(task.getReference(rowkeyRef)).toString();
			}
			else
			{
				rowkeyType = CellValueType.STRING.toString();
			}
		}

		return CellValueType.valueOf(rowkeyType);
	}
}
