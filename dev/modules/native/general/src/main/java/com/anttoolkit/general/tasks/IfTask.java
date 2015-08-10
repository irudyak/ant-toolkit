package com.anttoolkit.general.tasks;

import java.text.*;
import java.util.*;

import org.apache.tools.ant.*;

public class IfTask
		extends GenericTask
		implements TaskContainer
{
	private static final String EQUAL_CONDITION_1 = "=";
	private static final String EQUAL_CONDITION_2 = "==";
	private static final String NOT_EQUAL_CONDITION_1 = "!=";
	private static final String NOT_EQUAL_CONDITION_2 = "<>";
	private static final String MORE_CONDITION = ">";
	private static final String LESS_CONDITION = "<";
	private static final String MORE_OR_EQUAL_CONDITION = ">=";
	private static final String LESS_OR_EQUAL_CONDITION = "<=";

	private String arg1 = null;
	private String arg2 = null;
	private boolean ignoreCase = false;
	private String format = null;
	private String locale = null;
	private String condition = null;
	private boolean echo = false;

	private boolean wasExecuted = false;

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

	private List<Task> tasks = new LinkedList<Task>();

	public void setArg1(String value)
	{
		arg1 = value;
	}

	public void setArg2(String value)
	{
		arg2 = value;
	}

	public void setEcho(boolean echo)
	{
		this.echo = echo;
	}

	public void setIgnoreCase(boolean ignore)
	{
		ignoreCase = ignore;
	}

	public void setFormat(String format)
	{
		this.format = format;
	}

	public void setLocale(String locale)
	{
		this.locale = locale;
	}

	public void setCondition(String condition)
	{
		this.condition = condition;
	}

	public void addTask(Task task)
	{
		if (task == null)
		{
			return;
		}

		tasks.add(task);
	}

	public void doWork()
			throws BuildException
	{
		wasExecuted = false;

		arg1 = ignoreCase ? arg1.toLowerCase() : arg1;
		arg2 = ignoreCase ? arg2.toLowerCase() : arg2;

		arg1 = arg1.trim();
		arg2 = arg2.trim();

		if (!isConditionSatisfied() || tasks.isEmpty())
		{
			return;
		}

		if (echo)
		{
			this.log("Perform tasks for condition: " + arg1 + " " + condition + " " + arg2);
		}

		for (Task task : tasks)
		{
			task.perform();
		}

		wasExecuted = true;
	}

	public boolean wasExecuted()
	{
		return wasExecuted;
	}

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

		if (condition == null)
		{
			throw new BuildException("condition is not specified");
		}
	}

	private boolean isConditionSatisfied()
	{
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

		try
		{
			Double dbl1 = Double.parseDouble(arg1);
			Double dbl2 = Double.parseDouble(arg2);
			return isConditionSatisfied(dbl1, dbl2, DoubleComparator.instance);
		}
		catch (NumberFormatException e) {}

		return isConditionSatisfied(arg1, arg2, StringComparator.instance);
	}

	private boolean isConditionSatisfied(Object arg1, Object arg2, Comparator comparator)
	{
		if (EQUAL_CONDITION_1.equals(condition) || EQUAL_CONDITION_2.equals(condition))
		{
			return comparator.compare(arg1, arg2) == 0;
		}

		if (NOT_EQUAL_CONDITION_1.equals(condition) || NOT_EQUAL_CONDITION_2.equals(condition))
		{
			return comparator.compare(arg1, arg2) != 0;
		}

		if (MORE_CONDITION.equals(condition))
		{
			return comparator.compare(arg1, arg2) > 0;
		}

		if (LESS_CONDITION.equals(condition))
		{

			return comparator.compare(arg1, arg2) < 0;
		}

		if (MORE_OR_EQUAL_CONDITION.equals(condition))
		{
			return comparator.compare(arg1, arg2) >= 0;
		}

		if (LESS_OR_EQUAL_CONDITION.equals(condition))
		{
			return comparator.compare(arg1, arg2) <= 0;
		}

		throw new BuildException("Incorrect condition specified: " + condition);
	}

}
