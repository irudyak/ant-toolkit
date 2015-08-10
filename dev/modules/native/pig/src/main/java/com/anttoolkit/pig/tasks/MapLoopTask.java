package com.anttoolkit.pig.tasks;

import java.text.*;
import java.util.*;

import org.joda.time.*;

import org.apache.pig.backend.executionengine.*;
import org.apache.pig.data.*;
import org.apache.tools.ant.*;

import com.anttoolkit.hadoop.tasks.hadoop.*;
import com.anttoolkit.pig.tasks.util.*;

public class MapLoopTask
		extends GenericHadoopTask
		implements TaskContainer
{
	private int fieldNumber = -1;
	private String keyProperty;
	private String typeProperty;
	private String valueProperty;
	private String dateFormat = "MM/dd/yyyy hh:mm:ss aaa";
	private String locale = Locale.US.toString();
	private DateFormat dateFormatter;

	private List<Task> tasks = new LinkedList<Task>();

	public void setFieldNumber(int fieldNumber)
	{
		this.fieldNumber = fieldNumber;
	}

	public void setKeyProperty(String property)
	{
		keyProperty = property;
	}

	public void setTypeProperty(String property)
	{
		typeProperty = property;
	}

	public void setValueProperty(String property)
	{
		valueProperty = property;
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
	public void addTask(Task task)
	{
		tasks.add(task);
	}

	@Override
	protected void doHadoopWork() throws BuildException
	{
		Map<String, Object> map = getMapForProcessing();

		ContextManager.setCurrentMapContext(map);

		try
		{
			for (String key : map.keySet())
			{
				if (keyProperty != null)
				{
					setPropertyThreadSafe(keyProperty, key);
				}

				if (typeProperty != null)
				{
					setPropertyThreadSafe(typeProperty, getType(map.get(key)));
				}

				Object value = map.get(key);
				if (value instanceof Map)
				{
					ContextManager.setCurrentMapContext((Map<String, Object>)value);
				}
				else if (value instanceof Tuple)
				{
					ContextManager.setCurrentTupleContext((Tuple)value);
				}
				else if (value instanceof DataBag)
				{
					ContextManager.setCurrentBagContext((DataBag) value);
				}
				else
				{
					populateValueProperty(value);
				}

				try
				{
					for (Task task : tasks)
					{
						task.perform();
					}
				}
				finally
				{
					if (value instanceof Map)
					{
						ContextManager.resetCurrentMapContext();
					}
					else if (value instanceof Tuple)
					{
						ContextManager.resetCurrentTupleContext();
					}
					else if (value instanceof DataBag)
					{
						ContextManager.resetCurrentBagContext();
					}
				}
			}
		}
		finally
		{
			ContextManager.resetCurrentMapContext();
		}
	}

	protected void hadoopValidate()
	{
		if (fieldNumber == -1 && ContextManager.getCurrentMap() == null)
		{
			throw new BuildException("Either tuple field number should be specified or " +
					getTaskName() + " task should be inside task container which provides " +
					"Map to execution context");
		}
	}

	private String getType(Object obj)
	{
		if (obj == null)
		{
			return "NULL";
		}

		if (obj instanceof Boolean)
		{
			return "BOOLEAN";
		}

		if (obj instanceof Byte)
		{
			return "BYTE";
		}

		if (obj instanceof Integer)
		{
			return "INTEGER";
		}

		if (obj instanceof Long)
		{
			return "LONG";
		}

		if (obj instanceof Float)
		{
			return "FLOAT";
		}

		if (obj instanceof Double)
		{
			return "DOUBLE";
		}

		if ((obj instanceof Date) || (obj instanceof DateTime))
		{
			return "DATETIME";
		}

		if (obj instanceof Byte[])
		{
			return "BYTEARRAY";
		}

		if (obj instanceof String)
		{
			return "CHARARRAY";
		}

		if (obj instanceof Map)
		{
			return "MAP";
		}

		if (obj instanceof DataBag)
		{
			return "BAG";
		}

		if (obj instanceof Tuple)
		{
			return "TUPLE";
		}

		return "UNKNOWN";
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

	private void populateValueProperty(Object value)
	{
		if (valueProperty == null)
		{
			return;
		}

		if (value instanceof Date)
		{
			setPropertyThreadSafe(valueProperty, getDateFormat().format((Date) value));
			return;
		}

		if (value instanceof DateTime)
		{
			setPropertyThreadSafe(valueProperty, getDateFormat().format(((DateTime)value).toDate()));
			return;
		}

		setPropertyThreadSafe(valueProperty, value.toString());
	}

	private Map<String, Object> getMapForProcessing()
	{
		if (fieldNumber == -1)
		{
			Map<String, Object> map = ContextManager.getCurrentMap();
			if (map == null)
			{
				throw new BuildException(this.getTaskName() + " should be put inside task container which provides tuple or map " +
						"to execution context");
			}

			return map;
		}

		Tuple tuple = ContextManager.getCurrentTuple();
		if (tuple == null)
		{
			throw new BuildException(this.getTaskName() + " should be put inside task container which provides tuple," +
					" to be able to get map from tuple field " + fieldNumber);
		}

		try
		{
			if (tuple.getType(fieldNumber) != DataType.MAP ||
				!(tuple.get(fieldNumber) instanceof Map))
			{
				throw new BuildException("Tuple field " + fieldNumber + " doesn't represent a Map");
			}

			return (Map<String, Object>)tuple.get(fieldNumber);

		}
		catch (ExecException e)
		{
			throw new BuildException("Failed to get map from tuple field " + fieldNumber, e);
		}
	}
}
