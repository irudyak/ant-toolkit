package com.anttoolkit.general.tasks.time;

import java.text.*;
import java.util.*;

import com.anttoolkit.general.tasks.GenericTask;
import org.apache.tools.ant.*;

public class TimeDifferenceTask
		extends GenericTask
{
	private static final String RAW_FORMAT = "RAW";
	private static final DecimalFormat DECIMAL_FORMAT = new DecimalFormat("#.##");

	private static final String MILLISECONDS = "ms";
	private static final String SECONDS = "s";
	private static final String MINUTES = "m";
	private static final String HOURS = "h";

	private String time1 = null;
	private String time2 = null;
	private String format = null;
	private Locale locale = Locale.getDefault();
	private String differenceUnits = SECONDS;
	private String property = null;

	public void setTime1(String time)
	{
		time1 = time;
	}

	public void setTime2(String time)
	{
		time2 = time;
	}

	public void setTimeFormat(String format)
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

	public void setDifferenceUnits(String units)
	{
		differenceUnits = units.trim().toLowerCase();
	}

	public void setProperty(String property)
	{
		this.property = property;
	}

	public void doWork() throws BuildException
	{
		if (property != null)
		{
			setPropertyThreadSafe(property, getDifferenceInUnits(getTimeDifference()));
		}
	}

	protected void validate()
	{
		if (format == null)
		{
			throw new BuildException("Time format should be specified");
		}

		if (time1 == null)
		{
			throw new BuildException("time1 argument couldn't be null");
		}

		if (time2 == null)
		{
			throw new BuildException("time2 argument couldn't be null");
		}

		if (!MILLISECONDS.equals(differenceUnits) && !SECONDS.equals(differenceUnits) &&
			!MINUTES.equals(differenceUnits) && !HOURS.equals(differenceUnits))
		{
			throw new BuildException("Incorrect difference units specified: " + differenceUnits);
		}
	}

	private long getTimeDifference()
	{
		if (RAW_FORMAT.equals(format))
		{
			long time1;
			long time2;

			try
			{
				time1 = Long.parseLong(this.time1);
			}
			catch (Throwable e)
			{
				throw new BuildException("Failed to parse time1: " + this.time1);
			}

			try
			{
				time2 = Long.parseLong(this.time2);
			}
			catch (Throwable e)
			{
				throw new BuildException("Failed to parse time2: " + this.time2);
			}

			return time2 - time1;
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

		Date time1;
		Date time2;

		try
		{
			time1 = dateFormat.parse(this.time1.toUpperCase());
		}
		catch (Throwable e)
		{
			throw new BuildException("Failed to parse time1: " + this.time1);
		}

		try
		{
			time2 = dateFormat.parse(this.time2.toUpperCase());
		}
		catch (Throwable e)
		{
			throw new BuildException("Failed to parse time2: " + this.time2);
		}

		return time2.getTime() - time1.getTime();
	}

	private String getDifferenceInUnits(long milliseconds)
	{
		Float dirrefence = null;

		if (MILLISECONDS.equals(differenceUnits))
		{
			dirrefence = (float)milliseconds;
		}

		if (SECONDS.endsWith(differenceUnits))
		{
			dirrefence = (float)milliseconds / (float)1000;
		}

		if (MINUTES.endsWith(differenceUnits))
		{
			dirrefence = (float)milliseconds / (float)60000;
		}

		if (HOURS.endsWith(differenceUnits))
		{
			dirrefence = (float)milliseconds / (float)3600000;
		}

		if (dirrefence == null)
		{
			throw new BuildException("Incorrect difference units specified: " + differenceUnits);
		}

		return DECIMAL_FORMAT.format(dirrefence);
	}
}
