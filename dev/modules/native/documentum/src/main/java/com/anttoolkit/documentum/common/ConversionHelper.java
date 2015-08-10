package com.anttoolkit.documentum.common;

import com.documentum.fc.common.*;
import com.documentum.fc.client.*;

public class ConversionHelper
{
	public static boolean isEmptyString(String str)
	{
		return str == null || str.trim().length() == 0;
	}

	public static boolean isNullId(IDfId id)
	{
		return id == null || id.isNull();
	}

	public static boolean isNullId(String id)
	{
		return isEmptyString(id) || id.equals(DfId.DF_NULLID_STR);
	}

	public static Object convertToPrimaryObject(IDfValue value)
	{
		if (value == null)
		{
			return null;
		}

		switch (value.getDataType())
		{
			case IDfValue.DF_BOOLEAN:
				//noinspection UnnecessaryBoxing
				return Boolean.valueOf(value.asBoolean());
			case IDfValue.DF_INTEGER:
				return Integer.valueOf(value.asString());
			case IDfValue.DF_DOUBLE:
				return Double.valueOf(value.asString());
			case IDfValue.DF_ID:
				return value.asId();
			case IDfValue.DF_STRING:
				return value.asString();
			case IDfValue.DF_TIME:
				return value.asTime();
		}

		return null;
	}

	public static IDfId convertToDfId(Object value)
	{
		IDfId idValue = DfId.DF_NULLID;
		if (value != null && !ConversionHelper.isEmptyString(value.toString()))
		{
			idValue = new DfId(value.toString());
		}

		if (!idValue.isNull() && !idValue.isObjectId())
		{
			throw new IllegalArgumentException("Incorrect ID \"" + value + "\" value was specified");
		}

		return idValue;
	}

	public static boolean convertToBoolean(Object value)
	{
		if (value == null)
		{
			throw new IllegalArgumentException("Argument is empty");
		}

		if (value instanceof Boolean)
		{
			return (Boolean)value;
		}

		if (value instanceof IDfValue)
		{
			int type = ((IDfValue)value).getDataType();
			if (type != DfType.DF_BOOLEAN)
			{
				throw new IllegalArgumentException("IDfType argument has incorrect datatype=" + type);
			}

			return ((IDfValue)value).asBoolean();
		}

		String temp = value.toString();
		if (isEmptyString(temp))
		{
			throw new IllegalArgumentException("Invalid argument=" + value);
		}

		temp = temp.trim().toLowerCase();

		if (temp.equals("false") || temp.equals("no") || temp.equals("0"))
		{
			return false;
		}

		if (temp.equals("true") || temp.equals("yes") || temp.equals("1"))
		{
			return true;
		}

		throw new IllegalArgumentException("Invalid argument=" + value);
	}

	public static double convertToDouble(Object value)
	{
		if (value == null)
		{
			throw new IllegalArgumentException("Failed to set double property, because its value is NULL");
		}

		try
		{
			return Double.parseDouble(value.toString());
		}
		catch (NumberFormatException e)
		{
			throw new IllegalArgumentException("Incorrect double \"" + value + "\" value was specified");
		}
	}

	public static int convertToInt(Object value)
	{
		if (value == null)
		{
			throw new IllegalArgumentException("Failed to set integer property, because its value is NULL");
		}

		try
		{
			return Integer.parseInt(value.toString());
		}
		catch (NumberFormatException e)
		{
			throw new IllegalArgumentException("Incorrect integer \"" + value + "\" value was specified");
		}
	}
}
