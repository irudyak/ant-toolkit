package com.anttoolkit.general.tasks.time;

import java.text.*;
import java.util.*;

import com.anttoolkit.general.tasks.*;
import org.apache.tools.ant.*;

public class AddTimeTask extends GenericTask
{
	private static final String RAW_FORMAT = "RAW";

	private static final Map<String, Integer> UNITS = new HashMap<String, Integer>()
	{{
		this.put("year", Calendar.YEAR);
		this.put("month", Calendar.MONTH);
		this.put("week", Calendar.WEEK_OF_YEAR);
		this.put("day", Calendar.DATE);
		this.put("hour", Calendar.HOUR_OF_DAY);
		this.put("minute", Calendar.MINUTE);
		this.put("second", Calendar.SECOND);
		this.put("millisecond", Calendar.MILLISECOND);
	}};

	private String time;
	private String format;
	private Locale locale = Locale.getDefault();
	private String outFormat;
	private Locale outLocale = Locale.getDefault();
	private Integer unit;
	private Integer value;
	private String property;

	public void setTime(String time)
	{
		this.time = time;
	}

	public void setFormat(String format)
	{
		this.format = format;
	}

	public void setLocale(String locale)
	{
		String[] parts = locale.split(",", -1);
		if (parts.length != 2)
		{
			throw new BuildException("Invalid locale specified: " + locale);
		}

		this.locale = new Locale(parts[0], parts[1]);
	}

	public void setOutFormat(String format)
	{
		this.outFormat = format;
	}

	public void setOutLocale(String locale)
	{
		String[] parts = locale.split(",", -1);
		if (parts.length != 2)
		{
			throw new BuildException("Invalid locale specified: " + locale);
		}

		this.outLocale = new Locale(parts[0], parts[1]);
	}

	public void setUnit(String unit)
	{
		if (unit == null)
		{
			throw new BuildException("Calendar unit to add value to, can't be empty");
		}

		if (!UNITS.containsKey(unit.toLowerCase().trim()))
		{
			throw new BuildException("Incorrect calendar unit to add value to specified: " + unit);
		}

		this.unit = UNITS.get(unit.toLowerCase().trim());
	}

	public void setValue(int value)
	{
		this.value = value;
	}

	public void setProperty(String property)
	{
		this.property = property;
	}

	@Override
	public void doWork() throws BuildException
	{
		Date time = parseTime();
		Calendar calendar = Calendar.getInstance();
		calendar.setTime(time);

		calendar.add(unit, value);

		SimpleDateFormat formatter = new SimpleDateFormat(outFormat, outLocale);

		this.setPropertyThreadSafe(property, formatter.format(calendar.getTime()));
	}

	protected void validate()
	{
		if (time == null)
		{
			throw new BuildException("Time should be specified");
		}

		if (format == null)
		{
			throw new BuildException("Input time format should be specified");
		}

		if (outFormat == null)
		{
			throw new BuildException("Output time format should be specified");
		}

		if (unit == null)
		{
			throw new BuildException("Unit should be specified");
		}

		if (value == null)
		{
			throw new BuildException("Value should be specified");
		}

		if (property == null)
		{
			throw new BuildException("Property should be specified");
		}
	}

	private Date parseTime()
	{
		if (RAW_FORMAT.equals(format))
		{
			try
			{
				return new Date(Long.parseLong(time));
			}
			catch (Throwable e)
			{
				throw new BuildException("Failed to parse time: " + this.time);
			}
		}

		SimpleDateFormat dateFormat;

		try
		{
			dateFormat = new SimpleDateFormat(format, locale);
		}
		catch (IllegalArgumentException e)
		{
			throw new BuildException("Invalid time format specified: " + format);
		}

		try
		{
			return dateFormat.parse(this.time.toUpperCase());
		}
		catch (Throwable e)
		{
			throw new BuildException("Failed to parse time: " + this.time);
		}
	}
}
