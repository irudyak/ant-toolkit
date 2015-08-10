package com.anttoolkit.jasperreports.report;

import com.anttoolkit.general.entities.*;
import org.apache.tools.ant.*;

import java.util.*;

public class DataSourceManager
{
	private static final IEntityProcessor ENTITY_PROCESSOR = new IEntityProcessor<Collection<Object>, IDataSourceProcessor, Object> ()
	{
		@Override
		public Object processEntity(Collection<Object> dataset, IDataSourceProcessor processor)
		{
			return processor.processDataSource(dataset);
		}

		@Override
		public boolean readOnly()
		{
			return true;
		}
	};

	public static void initDataset(String name, Collection<Object> dataset)
	{
		try
		{
			EntityManager.setEntity(DataSourceCollectionEntityType.instance, name, dataset, false);
		}
		catch (EntityStorageAbsentException e)
		{
			throw new BuildException("There are no storage for DataSource data", e);
		}
	}

	public static void destroyDataset(String name)
	{
		EntityManager.removeEntity(DataSourceCollectionEntityType.instance, name);
	}

	public static boolean exists(String name)
	{
		return EntityManager.exists(DataSourceCollectionEntityType.instance, name);
	}

	public static Object processDataSource(String name, IDataSourceProcessor processor)
	{
		try
		{
			return EntityManager.processEntity(DataSourceCollectionEntityType.instance, name, ENTITY_PROCESSOR, processor);
		}
		catch (EntityNotFoundException e)
		{
			throw new BuildException("There was no DataSource " + name + " previously initialized", e);
		}
	}
}
