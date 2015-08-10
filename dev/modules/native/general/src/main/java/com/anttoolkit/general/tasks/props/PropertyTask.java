package com.anttoolkit.general.tasks.props;

import java.lang.reflect.*;
import java.text.*;
import java.util.*;

import org.apache.tools.ant.*;

import com.anttoolkit.general.common.*;
import com.anttoolkit.general.tasks.*;

public class PropertyTask
	extends GenericTask
{
	private String name = null;
	private String value = null;
	private String ref = null;
	private String refMethod = null;
	private String file = null;
	private int line = -1;

	private String timeFormat = null;
	private String locale = Locale.US.toString();

	public void setTimeFormat(String format)
	{
		timeFormat = format;
	}

	public void setLocale(String locale)
	{
		this.locale = locale;
	}

	public void setName(String name)
	{
		this.name = name;
	}

	public void setValue(String value)
	{
		this.value = value;
	}

	public void setRefid(String reference)
	{
		this.ref = reference;
	}

	public void setRefMethod(String method)
	{
		refMethod = method;
	}

	public void setFile(String file)
	{
		this.file = file;
	}

	public void setLine(int line)
	{
		this.line = line;
	}

	public void doWork() throws BuildException
	{
		if (value != null)
		{
			setPropertyThreadSafe(name, value);
			return;
		}

		if (file == null)
		{
			setPropertyThreadSafe(name, getReferencedObjectVal());
			return;
		}

		String val = loadFileContent(file);
		if (line >= 0)
		{
			String[] chunks = val.split("\n", -1);
			if (chunks.length - 1 < line)
			{
				throw new BuildException("File '" + file + "' doesn't contain line " + line);
			}

			val = chunks[line].trim();
		}

		setPropertyThreadSafe(name, val);
	}

	protected void validate()
	{
		if (name == null)
		{
			throw new BuildException("Property name should be specified");
		}

		if (value == null && ref == null && file == null)
		{
			throw new BuildException("Property value or reference or file should be specified");
		}

		if (value != null && ref != null && file != null)
		{
			throw new BuildException("Either property value or reference or file should be specified");
		}

		if (ref != null && this.getReference(ref) == null)
		{
			throw new BuildException("There is no referenced object associated with reference " + ref);
		}

		if (file != null && !this.fileExists(file))
		{
			throw new BuildException("Specified file '" + file + "' doesn't exist");
		}
	}

	private String getReferencedObjectVal()
	{
		Object obj = this.getReference(ref);
		if (refMethod == null)
		{
			return obj.toString();
		}

		try
		{
			Object value = ReflectionHelper.invokeMethod(obj, refMethod);
			String val = value == null || value.toString() == null ? "" : value.toString();

			if (timeFormat == null)
			{
				return val;
			}

			// empty or NULL time specified
			if (val.trim().isEmpty() || val.trim().equals("0"))
			{
				return "";
			}

			SimpleDateFormat format;

			try
			{
				format = new SimpleDateFormat(timeFormat, new Locale(locale));
			}
			catch (Throwable e)
			{
				throw new BuildException("Incorrect time format/locale specified: " + timeFormat + "/" + locale, e);
			}

			if (value instanceof Date)
			{
				return format.format((Date)value);
			}

			if (value instanceof Long)
			{
				return format.format(new Date((Long)value));
			}

			if (value instanceof Calendar)
			{
				return format.format(((Calendar)value).getTime());
			}

			throw new BuildException("The method " + refMethod + " of the reference " +
					ref + " didn't return value which could be recognized as a date: " + value);
		}
		catch (NoSuchMethodException e)
		{
			throw new BuildException("Object specified by reference " + ref + ", doesn't have " + refMethod + " method");
		}
		catch (InvocationTargetException e)
		{
			throw new BuildException("Failed to invoke " + refMethod + " method of referenced object " + ref, e);
		}
		catch (IllegalAccessException e)
		{
			throw new BuildException("Failed to invoke " + refMethod + " method of referenced object " + ref, e);
		}
	}
}
