package com.anttoolkit.documentum.common;

import com.anttoolkit.general.common.*;

import com.documentum.fc.common.*;
import com.documentum.fc.client.*;

import java.util.*;

import org.apache.tools.ant.*;

public class DocbaseObjectProperty
{
	private String name = null;
	private String value = null;
	private String query = null;
	private String format = IDfTime.DF_TIME_PATTERN_DEFAULT;

	private int dataType = IDfAttr.DM_UNDEFINED;
	private int length = -1;
	private boolean isRepeating = false;
	private boolean isMetadataInitialized = false;

	private Object resolvedValue = null;

	public DocbaseObjectProperty(){}

	public DocbaseObjectProperty(String propertyName,
								 String propertyValue)
	{
		setName(propertyName);
		setValue(propertyValue);
	}

	public void setName(String name)
	{
		this.name = name;
	}

	public String getName()
	{
		return name;
	}

	public void setValue(String value)
	{
		this.value = value;
	}

	public String getValue()
	{
		return value;
	}

	public void setQuery(String query)
	{
		this.query = query;
	}

	public String getQuery()
	{
		return query;
	}

	public void setFormat(String format)
	{
		this.format = format;
	}

	public String getFormat()
	{
		return format;
	}

	boolean isMetadataInitialized()
	{
		return isMetadataInitialized;
	}

	void setMetadata(int dataType,
							int length,
							boolean isRepeatin)
	{
		this.dataType = dataType;
		this.length = length;
		isRepeating = isRepeatin;
		isMetadataInitialized = true;
	}

	public void setDataType(int type)
	{
		dataType = type;
	}

	public int getDataType()
	{
		return dataType;
	}

	public int getLength()
	{
		return length;
	}

	public boolean isRepeating()
	{
		return isRepeating;
	}

	public Object resolvePropertyValue(DocbaseSession session)
			throws BuildException
	{
		if (resolvedValue != null)
		{
			return resolvedValue;
		}

		if (value != null)
		{
			resolvedValue = getSimpleValue();
		}
		else if (query != null)
		{
			resolvedValue = getValueFromQuery(session);
		}
		else
		{
			return null;
		}

		return resolvedValue;
	}

	private Object getValueFromQuery(DocbaseSession session)
			throws BuildException
	{
		try
		{
			switch (dataType)
			{
				case IDfType.DF_BOOLEAN:
					return BooleanHelper.getBoxedBoolean(DqlHelper.getBooleanParamFromFirstString(session, query));
				case IDfType.DF_DOUBLE:
					return new Double(DqlHelper.getDoubleParamFromFirstString(session, query));
				case IDfType.DF_ID:
					return DqlHelper.getIdParamFromFirstString(session, query);
				case IDfType.DF_INTEGER:
					return new Integer(DqlHelper.getIntegerParamFromFirstString(session, query));
				case IDfType.DF_STRING:
					return DqlHelper.getStringParamFromFirstString(session, query);
				case IDfType.DF_TIME:
					return DqlHelper.getTimeParamFromFirstString(session, query);
			}

			DqlHelper.getParamFromFirstString(session, this.getQuery());
		}
		catch (DfEndOfCollectionException e)
		{
			throw new BuildException("No results returned by the query for the property " + this.getName() +
			", query: " + this.getQuery());
		}
		catch (DfException e)
		{
			throw new BuildException("Failed to execute query", e);
		}

		throw new BuildException("Undefined datatype for property " + name);
	}

	private Object getSimpleValue()
			throws BuildException
	{
		switch (dataType)
		{
			case IDfType.DF_BOOLEAN:
				return BooleanHelper.getBoxedBoolean(value);
			case IDfType.DF_DOUBLE:
				return new Double(value);
			case IDfType.DF_ID:
				return getIDValue();
			case IDfType.DF_INTEGER:
				return new Integer(value);
			case IDfType.DF_STRING:
				return value;
			case IDfType.DF_TIME:
				return getTimeValue();
		}

		throw new BuildException("Undefined datatype for property " + name);
	}

	private IDfId getIDValue()
	{
		if (value.equals(DfId.DF_NULLID_STR) || value.trim().toUpperCase().equals("NULL"))
		{
			return DfId.DF_NULLID;
		}

		return new DfId(value);
	}

	private IDfTime getTimeValue()
	{
		String newValue = value.trim().toUpperCase();
		if (newValue.equals("TODAY"))
		{
			return new DfTime(getCurrentCalendar().getTime());
		}
		else if (newValue.equals("NOW"))
		{
			return new DfTime();
		}
		else if (newValue.equals("YESTERDAY"))
		{
			Calendar calendar = getCurrentCalendar();
			calendar.add(Calendar.DATE, -1);
			return new DfTime(calendar.getTime());
		}
		else if (newValue.equals("TOMORROW"))
		{
			Calendar calendar = getCurrentCalendar();
			calendar.add(Calendar.DATE, 1);
			return new DfTime(calendar.getTime());
		}
		else if (newValue.equals("NULL"))
		{
			return DfTime.DF_NULLDATE;
		}

		return new DfTime(value, format);
	}

	private Calendar getCurrentCalendar()
	{
		Calendar calendar = Calendar.getInstance();
		calendar.clear(Calendar.MILLISECOND);
		calendar.clear(Calendar.SECOND);
		calendar.clear(Calendar.MINUTE);
		calendar.clear(Calendar.HOUR_OF_DAY);
		calendar.clear(Calendar.HOUR);

		return calendar;
	}
}
