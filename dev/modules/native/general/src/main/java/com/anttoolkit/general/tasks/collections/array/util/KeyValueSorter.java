package com.anttoolkit.general.tasks.collections.array.util;

import java.util.*;

import org.apache.tools.ant.*;

import com.anttoolkit.general.entities.*;

public class KeyValueSorter extends ArrayProcessor<Object, Void>
{
	private static final IEntityProcessor SORTER = new Sorter();

	@Override
	public Void processEntity(List<String> entity, Object param)
	{
		if (!(param instanceof Object[]) || ((Object[])param).length != 2 ||
			!(((Object[])param)[0] instanceof Boolean) ||
			!(((Object[])param)[1] instanceof String))
		{
			throw new BuildException("Incorrect parameter specified: " + param);
		}

		Boolean ascending = (Boolean)(((Object[])param)[0]);
		String keysArray = (String)((Object[])param)[1];

		try
		{
			EntityManager.processEntity(ArrayEntityType.instance, keysArray, SORTER, new Object[]{ascending, entity});
		}
		catch (EntityNotFoundException e)
		{
			throw new BuildException("There was no array " + keysArray + " previously initialized", e);
		}

		return null;
	}

	@Override
	public boolean readOnly()
	{
		return false;
	}
}
