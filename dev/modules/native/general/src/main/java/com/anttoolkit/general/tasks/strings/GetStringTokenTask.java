package com.anttoolkit.general.tasks.strings;

import java.util.*;

import org.apache.tools.ant.*;

import com.anttoolkit.general.tasks.*;
import com.anttoolkit.general.tasks.strings.util.*;

public class GetStringTokenTask extends GenericTask
{
	private String string;
	private String separator = ",";
	private String property;
	private boolean trim = false;
	private int index = -1;
	private String defaultValue;
	private List<StringToken> tokens = new LinkedList<StringToken>();

	public void setString(String string)
	{
		this.string = string;
	}

	public void setSeparator(String separator)
	{
		this.separator = separator;
	}

	public void setProperty(String property)
	{
		this.property = property;
	}

	public void setTrim(boolean trim)
	{
		this.trim = trim;
	}

	public void setIndex(int index)
	{
		this.index = index;
	}

	public void setDefault(String value)
	{
		this.defaultValue = value;
	}

	public void addConfiguredToken(StringToken token)
	{
		token.validate();
		tokens.add(token);
	}

	@Override
	public void doWork() throws BuildException
	{
		String[] chunks = string.split(separator, -1);

		if (property != null && index != -1)
		{
			this.setPropertyThreadSafe(property, chunks.length - 1 < index ? defaultValue : chunks[index]);
		}

		for (StringToken token : tokens)
		{
			String value = chunks.length - 1 < token.getIndex() ? token.getDefault() : chunks[token.getIndex()];
			value = value == null ? defaultValue : value;

			if (trim && value != null)
			{
				value = value.trim();
			}

			this.setPropertyThreadSafe(token.getProperty(), value);
		}
	}

	@Override
	protected void validate()
	{
		if (string == null)
		{
			throw new BuildException("String from which to get token should be specified");
		}

		if (separator == null)
		{
			throw new BuildException("Tokens separator can't be null");
		}

		if (property == null && tokens.isEmpty())
		{
			throw new BuildException("Property name should be specified");
		}

		if (index < 0 && tokens.isEmpty())
		{
			throw new BuildException("Token index should be specified");
		}
	}
}
