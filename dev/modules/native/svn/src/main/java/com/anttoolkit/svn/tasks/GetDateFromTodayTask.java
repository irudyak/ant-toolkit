package com.anttoolkit.svn.tasks;

import com.anttoolkit.general.tasks.GenericTask;
import org.apache.tools.ant.*;

import java.util.*;
import java.text.*;

public class GetDateFromTodayTask
	extends GenericTask
{
	private int daysToAdd = 0;
	private String property;

	private static final String DATE_FORMAT = "{0}-{1}-{2} 00:00:00";
	
	public void setDaysToAdd(int days)
	{
		daysToAdd = days;
	}

	public void setProperty(String name)
	{
		property = name;
	}

	public void doWork()
			throws BuildException
	{
		Calendar calendar = Calendar.getInstance();
		calendar.add(Calendar.DATE, daysToAdd);

		String year = Integer.toString(calendar.get(Calendar.YEAR));

		String month = Integer.toString(calendar.get(Calendar.MONTH) + 1);
		month = month.length() == 1 ? "0" + month : month;

		String day = Integer.toString(calendar.get(Calendar.DAY_OF_MONTH));
		day = day.length() == 1 ? "0" + day : day;

		this.setPropertyThreadSafe(property,
				MessageFormat.format(DATE_FORMAT, new String[]{year, month, day}));
	}

	protected void validate()
	{
		if (property == null || property.trim().length() == 0)
		{
			throw new BuildException("Property name doesn't specified");
		}
	}
}
