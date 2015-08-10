package com.anttoolkit.general.tasks.collections.map.util;

import java.util.*;
import java.util.AbstractMap.*;

import org.apache.tools.ant.*;

import com.anttoolkit.general.entities.*;

public class MapManager
{
	private static final IEntityProcessor SIZER = new Sizer();
	private static final IEntityProcessor ADDER = new Adder();
	private static final IEntityProcessor ELEMENT_LOOKUP = new ElementLookup();
	private static final IEntityProcessor CONTAINS_LOOKUP = new ContainsLookup();
	private static final IEntityProcessor CLEANER = new Cleaner();
	private static final IEntityProcessor COPIER = new Copier();
	private static final IEntityProcessor REMOVER = new Remover();

	public static void clear(String name, boolean silentIfNotExists)
	{
		if (name == null)
		{
			throw new BuildException("Array name couldn't be null");
		}


		try
		{
			EntityManager.processEntity(MapEntityType.instance, name, CLEANER, null);
		}
		catch (EntityNotFoundException e)
		{
			if (!silentIfNotExists)
			{
				throw new BuildException("There was no map " + name + " previously initialized", e);
			}
		}
	}

	public static boolean containsKey(String name, String key)
	{
		if (key == null)
		{
			throw new BuildException("Key can't be null");
		}

		try
		{
			return (Boolean)EntityManager.processEntity(MapEntityType.instance, name, CONTAINS_LOOKUP, key);
		}
		catch (EntityNotFoundException e)
		{
			throw new BuildException("There was no map " + name + " previously initialized", e);
		}
	}

	public static String get(String name, String key)
	{
		if (key == null)
		{
			throw new BuildException("Key can't be null");
		}

		try
		{
			return (String)EntityManager.processEntity(MapEntityType.instance, name, ELEMENT_LOOKUP, key);
		}
		catch (EntityNotFoundException e)
		{
			throw new BuildException("There was no map " + name + " previously initialized", e);
		}
	}

	public static String put(String name, String key, String value)
	{
		try
		{
			return (String)EntityManager.processEntity(MapEntityType.instance, name, ADDER, new SimpleEntry<String, String>(key, value));
		}
		catch (EntityNotFoundException e)
		{
			throw new BuildException("There was no map " + name + " previously initialized", e);
		}
	}

	public static int size(String name)
	{
		try
		{
			return (Integer)EntityManager.processEntity(MapEntityType.instance, name, SIZER, null);
		}
		catch (EntityNotFoundException e)
		{
			throw new BuildException("There was no map " + name + " previously initialized", e);
		}
	}

	public static void init(String name, Map<String, String> data, boolean ignoreIfExist)
	{
		try
		{
			EntityManager.setEntity(MapEntityType.instance, name, data, ignoreIfExist);
		}
		catch (EntityStorageAbsentException e)
		{
			throw new BuildException("There are no storage for map data", e);
		}
	}

	public static void destroy(String name)
	{
		EntityManager.removeEntity(MapEntityType.instance, name);
	}

	public static boolean exists(String name)
	{
		return EntityManager.exists(MapEntityType.instance, name);
	}

	public static void copy(String src, String dest)
	{
		try
		{
			EntityManager.processEntity(MapEntityType.instance, src, COPIER, dest);
		}
		catch (EntityNotFoundException e)
		{
			throw new BuildException("There was no map " + src + " previously initialized", e);
		}
	}

	public static void remove(String name, String key)
	{
		if (key == null)
		{
			throw new BuildException("Key can't be null");
		}

		try
		{
			EntityManager.processEntity(MapEntityType.instance, name, REMOVER, key);
		}
		catch (EntityNotFoundException e)
		{
			throw new BuildException("There was no map " + name + " previously initialized", e);
		}
	}
}
