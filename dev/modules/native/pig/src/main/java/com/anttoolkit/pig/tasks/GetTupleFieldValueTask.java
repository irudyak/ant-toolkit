package com.anttoolkit.pig.tasks;

import java.text.*;
import java.util.*;

import org.apache.pig.backend.executionengine.*;
import org.apache.pig.data.*;
import org.apache.tools.ant.*;

import com.anttoolkit.hadoop.tasks.hadoop.*;
import com.anttoolkit.pig.tasks.util.*;

public class GetTupleFieldValueTask
		extends GenericHadoopTask
{
	private String property;
	private int fieldNumber = -1;
	private String dateFormat;
	private String locale = Locale.US.toString();

	public void setFieldNumber(int fieldNumber)
	{
		this.fieldNumber = fieldNumber;
	}

	public void setProperty(String property)
	{
		this.property = property;
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
		Tuple tuple = ContextManager.getCurrentTuple();
		if (tuple == null)
		{
			throw new BuildException(this.getTaskName() + " task should be inside appropriate tuples loop task");
		}

		try
		{
			if (tuple.isNull(fieldNumber))
			{
				setPropertyThreadSafe(property, "");
				return;
			}

			byte type = tuple.getType(fieldNumber);
			Object val = tuple.get(fieldNumber);

			if (dateFormat != null &&
				(type == DataType.LONG || type == DataType.DATETIME))
			{
				try
				{
					if (type == DataType.LONG)
					{
						setPropertyThreadSafe(property, getDateFormat().format(new Date((Long)val)));
					}
					else
					{
						setPropertyThreadSafe(property, getDateFormat().format(((org.joda.time.DateTime) tuple.get(fieldNumber)).toDate()));
					}
				}
				catch (ClassCastException e)
				{
					throw new BuildException("Failed to cast tuple " + fieldNumber + " field value " + val.toString() + " to time", e);
				}

				return ;
			}

			setPropertyThreadSafe(property, val.toString());
		}
		catch (ExecException e)
		{
			throw new BuildException("Failed to get field " + fieldNumber + " type from tuple", e);
		}
	}

	protected void hadoopValidate()
	{
		if (property == null || property.trim().isEmpty())
		{
			throw new BuildException("Property to assign field type should be specified");
		}

		if (fieldNumber == -1)
		{
			throw new BuildException("Field number should be specified");
		}
	}

	private DateFormat getDateFormat()
	{
		try
		{
			return new SimpleDateFormat(dateFormat, new Locale(locale));
		}
		catch (Throwable e)
		{
			throw new BuildException("Incorrect time format/locale specified", e);
		}
	}
}
