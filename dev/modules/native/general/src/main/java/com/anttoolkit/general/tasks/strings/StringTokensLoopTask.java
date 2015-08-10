package com.anttoolkit.general.tasks.strings;

import java.util.*;

import org.apache.tools.ant.*;

import com.anttoolkit.general.tasks.*;

public class StringTokensLoopTask
		extends GenericTask
		implements TaskContainer
{
	private List<Task> tasks = new LinkedList<Task>();
	private String string;
	private String separator = ",";
	private String tokenProperty;
	private String numberProperty;
	private boolean trimToken = false;
	private boolean ignoreEmptyTokens = false;

	public void setString(String string)
	{
		this.string = string;
	}

	public void setSeparator(String separator)
	{
		this.separator = separator;
	}

	public void setTokenProperty(String property)
	{
		this.tokenProperty = property;
	}

	public void setNumberProperty(String property)
	{
		this.numberProperty = property;
	}

	public void setTrimToken(boolean trim)
	{
		this.trimToken = trim;
	}

	public void setIgnoreEmptyTokens(boolean ignore)
	{
		this.ignoreEmptyTokens = ignore;
	}

	@Override
	public void doWork() throws BuildException
	{
		String[] tokens = string.split(separator, -1);
		int i = -1;

		for (String token : tokens)
		{
			i++;

			if (ignoreEmptyTokens && token.trim().isEmpty())
			{
				continue;
			}

			this.setPropertyThreadSafe(tokenProperty, trimToken ? token.trim() : token);

			if (numberProperty != null)
			{
				this.setPropertyThreadSafe(numberProperty, Integer.toString(i));
			}

			for (Task task : tasks)
			{
				task.perform();
			}
		}
	}

	@Override
	public void addTask(Task task)
	{
		tasks.add(task);
	}

	@Override
	protected void validate()
	{
		if (string == null)
		{
			throw new BuildException("String which tokens to iterate through should be specified");
		}

		if (separator == null)
		{
			throw new BuildException("Separator should be specified");
		}

		if (tokenProperty == null)
		{
			throw new BuildException("Property to store token should be specified");
		}
	}
}
