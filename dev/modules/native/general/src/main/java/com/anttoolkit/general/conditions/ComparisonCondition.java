package com.anttoolkit.general.conditions;

import java.text.*;
import java.util.*;

import org.apache.tools.ant.*;
import org.apache.tools.ant.taskdefs.condition.*;

public abstract class ComparisonCondition implements Condition
{
	private String arg1;
	private String arg2;
	private boolean ignoreCase = false;
	private String format;
	private String locale;

	public enum Operation
	{
		EQUAL, NOT_EQUAL, MORE, LESS, MORE_OR_EQUAL, LESS_OR_EQUAL;
	}

	private static class DoubleComparator implements Comparator<Double>
	{
		public static final DoubleComparator instance = new DoubleComparator();

		private DoubleComparator() {}

		public int compare(Double arg1, Double arg2)
		{
			return arg1.compareTo(arg2);
		}
	}

	private static class StringComparator implements Comparator<String>
	{
		public static final StringComparator instance = new StringComparator();

		private StringComparator() {}

		public int compare(String arg1, String arg2)
		{
			return arg1.compareTo(arg2);
		}
	}

	private static class DateComparator implements Comparator<Date>
	{
		public static final DateComparator instance = new DateComparator();

		private DateComparator() {}

		public int compare(Date arg1, Date arg2)
		{
			return arg1.compareTo(arg2);
		}
	}

	public void setArg1(String arg)
	{
		arg1 = arg;
	}

	public String getArg1()
	{
		return arg1;
	}

	public void setArg2(String arg)
	{
		arg2 = arg;
	}

	public String getArg2()
	{
		return arg2;
	}

	public void setIgnoreCase(boolean ignore)
	{
		ignoreCase = ignore;
	}

	public boolean getIgnoreCase()
	{
		return ignoreCase;
	}

	public void setFormat(String format)
	{
		this.format = format;
	}

	public String getFormat()
	{
		return format;
	}

	public void setLocale(String locale)
	{
		this.locale = locale;
	}

	public String getLocale()
	{
		return locale;
	}

	@Override
	public boolean eval() throws BuildException
	{
		validate();

		arg1 = ignoreCase ? arg1.toLowerCase() : arg1;
		arg2 = ignoreCase ? arg2.toLowerCase() : arg2;

		arg1 = arg1.trim();
		arg2 = arg2.trim();

		return isConditionSatisfied();
	}

	public abstract Operation operation();

	protected void validate()
	{
		if (arg1 == null)
		{
			throw new BuildException("arg1 is not specified");
		}

		if (arg2 == null)
		{
			throw new BuildException("arg2 is not specified");
		}
	}

	protected boolean isConditionSatisfied()
	{
		//dates comparison
		if (format != null)
		{
			try
			{
				String[] parts = locale == null ? null : locale.split(",", -1);
				SimpleDateFormat formatter = parts == null ?
						new SimpleDateFormat(format) :
						new SimpleDateFormat(format, new Locale(parts[0], parts[1]));

				Date date1 = formatter.parse(arg1);
				Date date2 = formatter.parse(arg2);

				return isConditionSatisfied(date1, date2, DateComparator.instance);
			}
			catch (ParseException e)
			{
				throw new BuildException("One of date arguments has incorrect format: " + arg1 + ", " + arg2);
			}
		}

		//numbers comparison
		try
		{
			Double number1 = Double.parseDouble(arg1);
			Double number2 = Double.parseDouble(arg2);
			return isConditionSatisfied(number1, number2, DoubleComparator.instance);
		}
		catch (NumberFormatException e) {}

		//raw strings comparison
		return isConditionSatisfied(arg1, arg2, StringComparator.instance);
	}

	protected boolean isConditionSatisfied(Object arg1, Object arg2, Comparator comparator)
	{
		if (operation().equals(Operation.EQUAL))
		{
			return comparator.compare(arg1, arg2) == 0;
		}

		if (operation().equals(Operation.NOT_EQUAL))
		{
			return comparator.compare(arg1, arg2) != 0;
		}

		if (operation().equals(Operation.MORE))
		{
			return comparator.compare(arg1, arg2) > 0;
		}

		if (operation().equals(Operation.LESS))
		{

			return comparator.compare(arg1, arg2) < 0;
		}

		if (operation().equals(Operation.MORE_OR_EQUAL))
		{
			return comparator.compare(arg1, arg2) >= 0;
		}

		if (operation().equals(Operation.LESS_OR_EQUAL))
		{
			return comparator.compare(arg1, arg2) <= 0;
		}

		throw new BuildException("Incorrect condition specified: " + operation());
	}
}
