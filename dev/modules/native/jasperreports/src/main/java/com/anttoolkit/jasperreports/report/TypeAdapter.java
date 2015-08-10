package com.anttoolkit.jasperreports.report;

import java.text.*;
import java.util.*;

import org.apache.tools.ant.*;

public class TypeAdapter
{
	public enum DataType
	{
		BOOLEAN, DATE, DOUBLE, FLOAT, INTEGER, INT, LONG, SHORT, STRING
	}

	private DataType type = DataType.STRING;
	private String dateFormat = "MM/dd/yyyy hh:mm:ss aaa";
	private String locale = Locale.US.toString();
	private DateFormat format;

	public void setType(String type)
	{
		this.type = DataType.valueOf(type.trim().toUpperCase());
	}

	public void setFormat(String format)
	{
		dateFormat = format;
	}

	public void setLocale(String locale)
	{
		this.locale = locale;
	}

	protected Object convertToPrimaryObject(String value)
			throws IllegalArgumentException
	{
		switch (type)
		{
			case BOOLEAN:
				return parseBoolean(value);
			case DATE:
				return parseDate(value);
			case DOUBLE:
				return parseDouble(value);
			case FLOAT:
				return parseFloat(value);
			case INTEGER:
				return parseInteger(value);
			case INT:
				return parseInteger(value);
			case LONG:
				return parseLong(value);
			case SHORT:
				return parseShort(value);
			case STRING:
				return value;
		}

		throw new IllegalStateException("Illegal primitive type '" + type + "' specified");
	}

	private DateFormat getDateFormat()
	{
		if (format != null)
		{
			return format;
		}

		try
		{
			return format = new SimpleDateFormat(dateFormat, new Locale(locale));
		}
		catch (Throwable e)
		{
			throw new BuildException("Incorrect time format/locale specified", e);
		}
	}

	private Boolean parseBoolean(String value)
			throws IllegalArgumentException
	{
		if (value == null || value.trim().isEmpty())
		{
			throw new IllegalArgumentException("Can't convert null value to boolean");
		}

		String val = value.trim().toLowerCase();

		if (val.equals("false") || val.equals("no") || val.equals("0"))
		{
			return false;
		}

		if (val.equals("true") || val.equals("yes") || val.equals("1"))
		{
			return true;
		}

		throw new IllegalArgumentException("Invalid boolean value '" + value + "' specified");
	}

	private Date parseDate(String value)
			throws IllegalArgumentException
	{
		if (value == null || value.trim().length() == 0)
		{
			return null;
		}

		try
		{
			return getDateFormat().parse(value);
		}
		catch (ParseException e) {}

		try
		{
			return new Date(Long.parseLong(value));
		}
		catch (NumberFormatException e) {}

		throw new IllegalArgumentException("Invalid date value '" + value + "' specified");
	}

	private Double parseDouble(String value)
			throws IllegalArgumentException
	{
		if (value == null || value.trim().length() == 0)
		{
			return null;
		}

		try
		{
			return Double.parseDouble(value);
		}
		catch (NumberFormatException e)
		{
			throw new IllegalArgumentException("Invalid double value '" + value + "' specified");
		}
	}

	private Float parseFloat(String value)
			throws IllegalArgumentException
	{
		if (value == null || value.trim().length() == 0)
		{
			return null;
		}

		try
		{
			return Float.parseFloat(value);
		}
		catch (NumberFormatException e)
		{
			throw new IllegalArgumentException("Invalid float value '" + value + "' specified");
		}
	}

	private Integer parseInteger(String value)
			throws IllegalArgumentException
	{
		if (value == null || value.trim().length() == 0)
		{
			return null;
		}

		try
		{
			return Integer.parseInt(value);
		}
		catch (NumberFormatException e)
		{
			throw new IllegalArgumentException("Invalid int value '" + value + "' specified");
		}
	}

	private Long parseLong(String value)
			throws IllegalArgumentException
	{
		if (value == null || value.trim().length() == 0)
		{
			return null;
		}

		try
		{
			return Long.parseLong(value);
		}
		catch (NumberFormatException e)
		{
			throw new IllegalArgumentException("Invalid long value '" + value + "' specified");
		}
	}

	private Short parseShort(String value)
			throws IllegalArgumentException
	{
		if (value == null || value.trim().length() == 0)
		{
			return null;
		}

		try
		{
			return Short.parseShort(value);
		}
		catch (NumberFormatException e)
		{
			throw new IllegalArgumentException("Invalid short value '" + value + "' specified");
		}
	}
}
