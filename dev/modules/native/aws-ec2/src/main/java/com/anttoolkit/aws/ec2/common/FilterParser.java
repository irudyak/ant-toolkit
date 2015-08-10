package com.anttoolkit.aws.ec2.common;

import java.util.*;

import com.amazonaws.services.ec2.model.*;

import org.apache.tools.ant.*;

public class FilterParser
{
	public static List<Filter> parse(String value)
	{
		if (value == null || value.trim().isEmpty())
		{
			return null;
		}

		List<Filter> filters = new LinkedList<Filter>();

		String val = value.replaceAll("(?i)values=", "");
		String[] nameAndValues = val.split("(?i)name=");

		for (String nameValues : nameAndValues)
		{
			String[] chunks = nameValues.split(",");
			if (chunks.length < 2)
			{
				throw new BuildException("Incorrect filters specified: " + filters);
			}

			String name = chunks[0].trim();
			if (name.isEmpty())
			{
				throw new BuildException("Incorrect filters specified: " + filters);
			}

			List<String> values = new LinkedList<String>();
			for (int i = 1; i < chunks.length; i++)
			{
				if (chunks[i].trim().isEmpty())
				{
					throw new BuildException("Incorrect filters specified: " + filters);
				}

				values.add(chunks[i].trim());
			}

			filters.add(new Filter(name, values));
		}

		return filters;
	}
}
