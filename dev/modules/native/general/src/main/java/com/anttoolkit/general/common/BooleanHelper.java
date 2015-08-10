package com.anttoolkit.general.common;

public class BooleanHelper
{
	public static String toString(boolean value)
	{
		return value ? "true" : "false";
	}

	public static boolean getBoolean(String value)
	{
		if (value == null || value.trim().length() == 0)
		{
			throw new IllegalArgumentException("Incorrect boolean value specified: null");
		}

		String newValue = value.trim().toLowerCase();

		if (newValue.equals("true") ||
			newValue.equals("yes") ||
			newValue.equals("1") ||
			newValue.equals("on"))
		{
			return true;
		}

		if (newValue.equals("false") ||
			newValue.equals("no") ||
			newValue.equals("0") ||
			newValue.equals("off"))
		{
			return false;
		}

		throw new IllegalArgumentException("Incorrect boolean value specified: " + value);
	}

	/** @noinspection UnnecessaryBoxing*/
	public static Boolean getBoxedBoolean(String value)
	{
		return Boolean.valueOf(getBoolean(value));
	}

	/** @noinspection UnnecessaryBoxing*/
	public static Boolean getBoxedBoolean(boolean value)
	{
		return Boolean.valueOf(value);
	}
}
