package com.anttoolkit.general.tasks.strings;

import java.text.*;
import java.util.*;

import org.apache.tools.ant.*;

import com.anttoolkit.general.entities.*;
import com.anttoolkit.general.tasks.*;
import com.anttoolkit.general.tasks.collections.array.util.*;

public class MessageFormatTask
	extends GenericTask
	implements IEntityProcessor<List<String>, Void, Void>
{
	private String pattern;
	private String property;
	private String values;
	private String separator = ",";
	private String array;

	public void setPattern(String pattern)
	{
		this.pattern = pattern;
	}

	public void setProperty(String property)
	{
		this.property = property;
	}

	public void setValues(String values)
	{
		this.values = values;
	}

	public void setValuesSeparator(String separator)
	{
		this.separator = separator;
	}

	public void setArray(String array)
	{
		this.array = array;
	}

	public void doWork()
			throws BuildException
	{
		if (values != null)
		{
			this.setPropertyThreadSafe(property, MessageFormat.format(pattern, values.split(separator, -1)));
			return;
		}

		try
		{
			EntityManager.processEntity(ArrayEntityType.instance, array, this, null);
		}
		catch (EntityNotFoundException e)
		{
			throw new BuildException("Array " + array + " wasn't previously initialized", e);
		}
	}

	@Override
	public Void processEntity(List<String> data, Void param)
	{
		this.setPropertyThreadSafe(property, MessageFormat.format(pattern, data.toArray(new String[data.size()])));
		return null;
	}

	@Override
	public boolean readOnly()
	{
		return true;
	}

	protected void validate()
	{
		if (pattern == null || pattern.trim().length() == 0)
		{
			throw new BuildException("Pattern doesn't specified");	
		}

		if (property == null || property.trim().length() == 0)
		{
			throw new BuildException("Property name doesn't specified");	
		}

		if (array != null && values != null)
		{
			throw new BuildException("Only one, array name or values should be specified");
		}

		if (array == null && values == null)
		{
			throw new BuildException("Array name or array data should be specified");
		}
	}
}
