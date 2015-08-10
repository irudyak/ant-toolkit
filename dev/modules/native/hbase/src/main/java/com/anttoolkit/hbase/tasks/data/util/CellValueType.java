package com.anttoolkit.hbase.tasks.data.util;

import java.math.*;
import java.text.*;

import org.apache.hadoop.hbase.util.*;
import org.apache.tools.ant.*;

public enum CellValueType
{
	STRING, BYTES, INT, SHORT, LONG, FLOAT, DOUBLE, BOOLEAN, BIGDECIMAL;

	private static final String MSG_ENCODE = "Failed to encode provided String ''{0}'' to {1}";
	private static final String MSG_DECODE = "Failed to decode provided byte[] ''{0}'' to {1}";

	public static byte[] encodeObject(Object obj)
	{
		if (obj == null)
		{
			throw new IllegalArgumentException("Can't encode null object");
		}

		if (obj instanceof byte[])
		{
			return (byte[])obj;
		}
		else if (obj instanceof String)
		{
			return Bytes.toBytes((String)obj);
		}
		else if (obj instanceof Integer)
		{
			return Bytes.toBytes((Integer)obj);
		}
		else if (obj instanceof Short)
		{
			return Bytes.toBytes((Short)obj);
		}
		else if (obj instanceof Long)
		{
			return Bytes.toBytes((Long)obj);
		}
		else if (obj instanceof Float)
		{
			return Bytes.toBytes((Float)obj);
		}
		else if (obj instanceof Double)
		{
			return Bytes.toBytes((Double)obj);
		}
		else if (obj instanceof Boolean)
		{
			return Bytes.toBytes((Boolean)obj);
		}
		else if (obj instanceof BigDecimal)
		{
			return Bytes.toBytes((BigDecimal)obj);
		}
		else
		{
			return Bytes.toBytes(obj.toString());
		}
	}

	public static Object decodeObject(byte[] value)
	{
		try
		{
			return Bytes.toInt(value);
		}
		catch (Throwable e)
		{
		}

		try
		{
			return Bytes.toShort(value);
		}
		catch (Throwable e)
		{
		}

		try
		{
			return Bytes.toLong(value);
		}
		catch (Throwable e)
		{
		}

		try
		{
			return Bytes.toFloat(value);
		}
		catch (Throwable e)
		{
		}

		try
		{
			return Bytes.toDouble(value);
		}
		catch (Throwable e)
		{
		}

		try
		{
			return Bytes.toBoolean(value);
		}
		catch (Throwable e)
		{
		}

		try
		{
			return Bytes.toBigDecimal(value);
		}
		catch (Throwable e)
		{
		}

		return Bytes.toStringBinary(value);
	}

	public static CellValueType detectEncodingType(Object obj)
	{
		if (obj == null)
		{
			throw new IllegalArgumentException("Can't encode null object");
		}

		if (obj instanceof byte[])
		{
			return BYTES;
		}
		else if (obj instanceof String)
		{
			return STRING;
		}
		else if (obj instanceof Integer)
		{
			return INT;
		}
		else if (obj instanceof Short)
		{
			return SHORT;
		}
		else if (obj instanceof Long)
		{
			return LONG;
		}
		else if (obj instanceof Float)
		{
			return FLOAT;
		}
		else if (obj instanceof Double)
		{
			return DOUBLE;
		}
		else if (obj instanceof Boolean)
		{
			return BOOLEAN;
		}
		else if (obj instanceof BigDecimal)
		{
			return BIGDECIMAL;
		}
		else
		{
			return BYTES;
		}
	}

	public byte[] encode(String value)
	{
		if (value == null)
		{
			throw new BuildException("Can't encode null value to any of the supported HBase types");
		}

		switch (this)
		{
			case STRING:
				return Bytes.toBytes(value);
			case BYTES:
				return Bytes.toBytesBinary(value);
			case INT:
				try
				{
					return Bytes.toBytes(Integer.parseInt(value));
				}
				catch (NumberFormatException e)
				{
					throw new BuildException(MessageFormat.format(MSG_ENCODE, value, "int"), e);
				}
			case SHORT:
				try
				{
					return Bytes.toBytes(Short.parseShort(value));
				}
				catch (NumberFormatException e)
				{
					throw new BuildException(MessageFormat.format(MSG_ENCODE, value, "short"), e);
				}
			case LONG:
				try
				{
					return Bytes.toBytes(Long.parseLong(value));
				}
				catch (NumberFormatException e)
				{
					throw new BuildException(MessageFormat.format(MSG_ENCODE, value, "long"), e);
				}
			case FLOAT:
				try
				{
					return Bytes.toBytes(Float.parseFloat(value));
				}
				catch (NumberFormatException e)
				{
					throw new BuildException(MessageFormat.format(MSG_ENCODE, value, "float"), e);
				}
			case DOUBLE:
				try
				{
					return Bytes.toBytes(Double.parseDouble(value));
				}
				catch (NumberFormatException e)
				{
					throw new BuildException(MessageFormat.format(MSG_ENCODE, value, "double"), e);
				}
			case BOOLEAN:
				try
				{
					return Bytes.toBytes(Boolean.parseBoolean(value));
				}
				catch (Throwable e)
				{
					throw new BuildException(MessageFormat.format(MSG_ENCODE, value, "boolean"), e);
				}
			case BIGDECIMAL:
				try
				{
					return Bytes.toBytes(new BigDecimal(value));
				}
				catch (Throwable e)
				{
					throw new BuildException(MessageFormat.format(MSG_ENCODE, value, "BigDecimal"), e);
				}
		}

		throw new BuildException("Unsupported cell type specified: " + this);
	}

	public Object decode(byte[] value)
	{
		if (value == null)
		{
			throw new BuildException("Can't decode null value to any supported Java type");
		}

		switch (this)
		{
			case STRING:
				return Bytes.toStringBinary(value);
			case BYTES:
				return Bytes.toStringBinary(value);
			case INT:
				try
				{
					return Bytes.toInt(value);
				}
				catch (Throwable e)
				{
					throw new BuildException(MessageFormat.format(MSG_DECODE, Bytes.toStringBinary(value), "int"), e);
				}
			case SHORT:
				try
				{
					return Bytes.toShort(value);
				}
				catch (Throwable e)
				{
					throw new BuildException(MessageFormat.format(MSG_DECODE, Bytes.toStringBinary(value), "short"), e);
				}
			case LONG:
				try
				{
					return Bytes.toLong(value);
				}
				catch (Throwable e)
				{
					throw new BuildException(MessageFormat.format(MSG_DECODE, Bytes.toStringBinary(value), "long"), e);
				}
			case FLOAT:
				try
				{
					return Bytes.toFloat(value);
				}
				catch (Throwable e)
				{
					throw new BuildException(MessageFormat.format(MSG_DECODE, Bytes.toStringBinary(value), "float"), e);
				}
			case DOUBLE:
				try
				{
					return Bytes.toDouble(value);
				}
				catch (Throwable e)
				{
					throw new BuildException(MessageFormat.format(MSG_DECODE, Bytes.toStringBinary(value), "double"), e);
				}
			case BOOLEAN:
				try
				{
					return Bytes.toBoolean(value);
				}
				catch (Throwable e)
				{
					throw new BuildException(MessageFormat.format(MSG_DECODE, Bytes.toStringBinary(value), "boolean"), e);
				}
			case BIGDECIMAL:
				try
				{
					return Bytes.toBigDecimal(value);
				}
				catch (Throwable e)
				{
					throw new BuildException(MessageFormat.format(MSG_DECODE, Bytes.toStringBinary(value), "BigDecimal"), e);
				}
		}

		throw new BuildException("Unsupported cell type specified: " + this);
	}
}
