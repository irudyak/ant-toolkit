package com.anttoolkit.documentum.tasks.alias;

import org.apache.tools.ant.*;

import com.anttoolkit.documentum.common.*;

import com.documentum.fc.common.*;

import java.util.*;

public class GetAliasValueTask
		extends GenericDocbaseTask
{
	public class Alias
	{
		public void setName(String name)
		{
			this.name = name;
		}

		public String getName()
		{
			return name;
		}

		public void setProperty(String name)
		{
			property = name;
		}

		public String getProperty()
		{
			return property;
		}

		private String name = null;
		private String property = null;
	}

	private String aliasSet = null;
	private List<Alias> aliases = new LinkedList<Alias>();

	public Alias createAlias()
	{
		Alias alias = new Alias();
		aliases.add(alias);
		return alias;
	}

	public void setAliasSet(String aliasSet)
	{
		this.aliasSet = aliasSet;
	}

	public void doWork()
			throws BuildException
	{
		if (aliasSet == null)
		{
			throw new BuildException("Alias set name should be specified");
		}

		int count = aliases.size();
		if (count == 0)
		{
			return;
		}

		Map<String, String> values = null;

		try
		{
			values = AliasHelper.getAliasSetNameValuePairs(getSession(), aliasSet);
		}
		catch (DfException e)
		{
			throw new BuildException("Failed to get alias set '" + aliasSet + "' values");
		}
		catch (DfEndOfCollectionException e)
		{
			throw new BuildException("Alias set '" + aliasSet + "' doesn't exist");
		}

		if (values == null || values.size() != count)
		{
			throw new BuildException("Not all aliases exist in requested alias set '" + aliasSet + "'");
		}

		for (Alias alias : aliases)
		{
			if (!values.containsKey(alias.getName()))
			{
				throw new BuildException("Alias '" + alias.getName() + "' doesn't exist in requested alias set '" + aliasSet + "'");
			}

			this.setPropertyThreadSafe(alias.getName(), values.get(alias.getName()));
		}
	}
}
