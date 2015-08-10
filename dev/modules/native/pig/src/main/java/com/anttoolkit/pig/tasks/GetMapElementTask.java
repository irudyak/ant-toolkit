package com.anttoolkit.pig.tasks;

import com.anttoolkit.pig.tasks.util.*;
import org.apache.tools.ant.*;

import com.anttoolkit.hadoop.tasks.hadoop.*;
import org.joda.time.DateTime;

import java.text.*;
import java.util.*;

public class GetMapElementTask
		extends GenericHadoopTask
{
	private String key;
	private String property;
	private String dateFormat = "MM/dd/yyyy hh:mm:ss aaa";
	private String locale = Locale.US.toString();
	private DateFormat dateFormatter;

	public void setKey(String key)
	{
		this.key = key;
	}

	public void setProperty(String name)
	{
		property = name;
	}

	public void setDateFormat(String format)
	{
		dateFormat = format;
	}

	public void setLocale(String locale)
	{
		this.locale = locale;
	}

	@Override
	protected void doHadoopWork() throws BuildException
	{
		Map<String, Object> map = ContextManager.getCurrentMap();
		Object val = map.get(key);
		if (val == null)
		{
			throw new BuildException("Map doesn't contain values associated with " + key + " key");
		}

		if (val instanceof Date)
		{
			setPropertyThreadSafe(property, getDateFormat().format((Date)val));
		}
		else if (val instanceof DateTime)
		{
			setPropertyThreadSafe(property, getDateFormat().format(((DateTime)val).toDate()));
		}
		else
		{
			setPropertyThreadSafe(property, val.toString());
		}
	}

	@Override
	protected void hadoopValidate()
	{
		if (ContextManager.getCurrentMap() == null)
		{
			throw new BuildException(getTaskName() + " should be put inside task container which specifies map for execution context");
		}

		if (key == null)
		{
			throw new BuildException("Map key should be specified");
		}

		if (property == null)
		{
			throw new BuildException("Property name should be specified");
		}
	}

	private DateFormat getDateFormat()
	{
		if (dateFormatter != null)
		{
			return dateFormatter;
		}

		try
		{
			return dateFormatter = new SimpleDateFormat(dateFormat, new Locale(locale));
		}
		catch (Throwable e)
		{
			throw new BuildException("Incorrect time format/locale specified: " + dateFormat + "/" + locale, e);
		}
	}
}
