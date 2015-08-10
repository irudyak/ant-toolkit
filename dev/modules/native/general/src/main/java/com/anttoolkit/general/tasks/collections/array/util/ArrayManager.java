package com.anttoolkit.general.tasks.collections.array.util;

import java.util.*;

import org.apache.tools.ant.*;

import com.anttoolkit.general.entities.*;

public class ArrayManager
{
	private static final IEntityProcessor CLEANER = new Cleaner();
	private static final IEntityProcessor SORTER = new Sorter();
	private static final IEntityProcessor KEY_VALUE_SORTER = new KeyValueSorter();
	private static final IEntityProcessor CONTAINS_LOOKUP = new ContainsLookup();
	private static final IEntityProcessor ELEMENT_LOOKUP = new ElementLookup();
	private static final IEntityProcessor ADDER = new Adder();
	private static final IEntityProcessor SETTER = new Setter();
	private static final IEntityProcessor SIZER = new Sizer();
	private static final IEntityProcessor COPIER = new Copier();
	private static final IEntityProcessor REMOVER = new Remover();

	public static boolean contains(String name, String value)
	{
		try
		{
			return (Boolean)EntityManager.processEntity(ArrayEntityType.instance, name, CONTAINS_LOOKUP, value);
		}
		catch (EntityNotFoundException e)
		{
			throw new BuildException("There was no array " + name + " previously initialized", e);
		}
	}

	public static void clear(String name, boolean silentIfNotExists)
	{
		try
		{
			EntityManager.processEntity(ArrayEntityType.instance, name, CLEANER, null);
		}
		catch (EntityNotFoundException e)
		{
			if (!silentIfNotExists)
			{
				throw new BuildException("There was no array " + name + " previously initialized", e);
			}
		}
	}

	public static String get(String name, int index)
	{
		if (index < 0)
		{
			throw new BuildException("Invalid array index " + Integer.toString(index) + " specified");
		}

		try
		{
			return (String)EntityManager.processEntity(ArrayEntityType.instance, name, ELEMENT_LOOKUP, index);
		}
		catch (EntityNotFoundException e)
		{
			throw new BuildException("There was no array " + name + " previously initialized", e);
		}
	}

	public static void add(String name, String value)
	{
		try
		{
			EntityManager.processEntity(ArrayEntityType.instance, name, ADDER, value);
		}
		catch (EntityNotFoundException e)
		{
			throw new BuildException("There was no array " + name + " previously initialized", e);
		}
	}

	public static int size(String name)
	{
		try
		{
			return (Integer)EntityManager.processEntity(ArrayEntityType.instance, name, SIZER, null);
		}
		catch (EntityNotFoundException e)
		{
			throw new BuildException("There was no array " + name + " previously initialized", e);
		}
	}

	public static void init(String name, List<String> data, boolean ignoreIfAlreadyExist)
	{
		try
		{
			EntityManager.setEntity(ArrayEntityType.instance, name, data, ignoreIfAlreadyExist);
		}
		catch (EntityStorageAbsentException e)
		{
			throw new BuildException("There are no storage for array data", e);
		}
	}

	public static void destroy(String name)
	{
		EntityManager.removeEntity(ArrayEntityType.instance, name);
	}

	public static void sort(String name, boolean ascending)
	{
		try
		{
			EntityManager.processEntity(ArrayEntityType.instance, name, SORTER, ascending);
		}
		catch (EntityNotFoundException e)
		{
			throw new BuildException("There was no array " + name + " previously initialized", e);
		}
	}

	public static void sort(String keysArray, String valuesArray, boolean ascending)
	{
		try
		{
			EntityManager.processEntity(ArrayEntityType.instance, valuesArray, KEY_VALUE_SORTER, new Object[] {ascending, keysArray});
		}
		catch (EntityNotFoundException e)
		{
			throw new BuildException("There was no array " + valuesArray + " previously initialized", e);
		}
	}

	public static boolean exists(String name)
	{
		return EntityManager.exists(ArrayEntityType.instance, name);
	}

	public static void copy(String src, String dest)
	{
		try
		{
			EntityManager.processEntity(ArrayEntityType.instance, src, COPIER, dest);
		}
		catch (EntityNotFoundException e)
		{
			throw new BuildException("There was no array " + src + " previously initialized", e);
		}
	}

	public static void set(String name, int index, String value)
	{
		try
		{
			EntityManager.processEntity(ArrayEntityType.instance, name, SETTER, new Object[]{index, value});
		}
		catch (EntityNotFoundException e)
		{
			throw new BuildException("There was no array " + name + " previously initialized", e);
		}
	}

	public static void remove(String name, int index)
	{
		try
		{
			EntityManager.processEntity(ArrayEntityType.instance, name, REMOVER, index);
		}
		catch (EntityNotFoundException e)
		{
			throw new BuildException("There was no array " + name + " previously initialized", e);
		}
	}
}
