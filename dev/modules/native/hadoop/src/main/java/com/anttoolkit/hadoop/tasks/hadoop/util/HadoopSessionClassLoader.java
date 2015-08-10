package com.anttoolkit.hadoop.tasks.hadoop.util;

import java.io.*;
import java.util.*;

import com.anttoolkit.general.tasks.classload.*;
import com.anttoolkit.hadoop.types.*;
import com.anttoolkit.hadoop.types.Property;

public class HadoopSessionClassLoader
		extends ResourceClassLoader
{
	private HadoopConfig hadoopConf;
	private Map<String, String> substitutedProperties = new HashMap<String, String>();
	private boolean wasClassLoaderSubstituted = false;

	public HadoopSessionClassLoader(HadoopConfig hadoopConf, List<File> extraResources)
	{
		super(hadoopConf, extraResources);
		this.hadoopConf = hadoopConf;
	}

	public void substituteThreadClassLoader()
	{
		if (hadoopConf == null || checkThreadAlreadyHasSameClassLoader())
		{
			return;
		}

		super.substituteThreadClassLoader();

		for (Property prop : hadoopConf.getProperties())
		{
			String value = System.getProperties().getProperty(prop.getName());
			substitutedProperties.put(prop.getName(), value);
			System.getProperties().setProperty(prop.getName(), prop.getValue());
		}

		wasClassLoaderSubstituted = true;
	}

	public void restoreThreadClassLoader()
	{
		if (!wasClassLoaderSubstituted)
		{
			return;
		}

		super.restoreThreadClassLoader();

		for (String name : substitutedProperties.keySet())
		{
			String value = substitutedProperties.get(name);
			if (value == null)
			{
				System.getProperties().remove(name);
			}
			else
			{
				System.getProperties().setProperty(name, value);
			}
		}

		substitutedProperties.clear();
		wasClassLoaderSubstituted = false;
	}

	private boolean checkThreadAlreadyHasSameClassLoader()
	{
		ClassLoader loader = Thread.currentThread().getContextClassLoader();

		while (loader != null)
		{
			if ((loader instanceof HadoopSessionClassLoader) &&
				hadoopConf.equals(((HadoopSessionClassLoader)loader).hadoopConf))
			{
				return true;
			}

			loader = loader.getParent();
		}

		return false;
	}
}
