package com.anttoolkit.sql.tasks.build.util;

import java.sql.*;
import java.util.*;

import org.apache.tools.ant.*;

import com.anttoolkit.general.tasks.build.util.*;
import com.anttoolkit.general.tasks.*;
import com.anttoolkit.sql.common.*;

public class VersionProviderProxy
		implements IBuildVersionProvider
{
	private Map<String, String> properties;
	private IBuildVersionProvider versionProvider;
	private GenericTask task;

	@Override
	public void init(GenericTask task, Map<String, String> properties)
	{
		this.properties = properties;
		this.task = task;
	}

	@Override
	public void validate(GenericTask task)
	{
	}

	@Override
	public BuildVersion getCurrentVersion(String qualifier)
	{
		return getVersionProvider().getCurrentVersion(qualifier);
	}

	@Override
	public boolean canUpdateToVersion(BuildVersion version, String qualifier)
	{
		return getVersionProvider().canUpdateToVersion(version, qualifier);
	}

	@Override
	public void updateToVersion(BuildVersion currentVersion, BuildVersion newVersion, String comment, String qualifier)
	{
		getVersionProvider().updateToVersion(currentVersion, newVersion, comment, qualifier);
	}

	private IBuildVersionProvider getVersionProvider()
	{
		if (versionProvider != null)
		{
			return versionProvider;
		}

		//resolving version provider from current SQL context
		if (properties == null || !properties.containsKey(GenericSqlVersionProvider.DRIVER_PARAM))
		{
			Driver driver = SqlSessionManager.getSession().getLoginInfo().driver;
			return versionProvider = createVersionProvider(driver.getClass().getName());
		}

		//resolving version provider from specified properties
		versionProvider = createVersionProvider(properties.get(GenericSqlVersionProvider.DRIVER_PARAM));
		versionProvider.init(task, properties);

		return versionProvider;
	}

	private IBuildVersionProvider createVersionProvider(String driverClass)
	{
		if (driverClass.contains("oracle"))
		{
			return new OracleVersionProvider();
		}

		if (driverClass.contains("mysql"))
		{
			return new MySqlVersionProvider();
		}

		throw new BuildException("There are no build version provider for database driver: " + driverClass);
	}
}
