package com.anttoolkit.general.refs;

import org.apache.tools.ant.*;

import com.anttoolkit.general.entities.*;

public class ReferenceManager
{
	private static class NullValue
	{
		public static final NullValue instance = new NullValue();

		private NullValue()
		{
		}
	}

	private static final IEntityProcessor PROCESSOR = new IEntityProcessor<Object, Void, Object> ()
	{
		@Override
		public Object processEntity(Object ref, Void param)
		{
			return ref;
		}

		@Override
		public boolean readOnly()
		{
			return false;
		}
	};

	public static void setReference(String name, Object obj, Project project)
	{
		try
		{
			EntityManager.setEntity(ReferenceEntityType.instance, name, obj == null ? NullValue.instance : obj, false);
		}
		catch (EntityStorageAbsentException e)
		{
			project.addReference(name, obj);
		}
		catch (ParentScopeAbsentException e)
		{
			//accessing 'root' references which are stored not in 'Entity Store', but in Ant project
			if (e.parentLevel - e.scopeLevel == 1)
			{
				project.addReference(EntityManager.removeParentScopePrefix(name), obj);
			}

			throw e;
		}
	}

	public static Object getReference(String name, Project project)
	{
		try
		{
			Object obj = EntityManager.processEntity(ReferenceEntityType.instance, name, PROCESSOR, null);
			return obj instanceof NullValue ? null : obj;
		}
		catch (EntityNotFoundException e)
		{
			return project.hasReference(name) ? project.getReference(name) : null;
		}
		catch (ParentScopeAbsentException e)
		{
			//accessing 'root' references which are stored not in 'Entity Store', but in Ant project
			if (e.parentLevel - e.scopeLevel == 1)
			{
				String _name = EntityManager.removeParentScopePrefix(name);
				return project.hasReference(_name) ? project.getReference(_name) : null;
			}

			throw e;
		}
	}

	public static boolean exists(String name, Project project)
	{
		boolean exists = false;

		try
		{
			exists = EntityManager.exists(ReferenceEntityType.instance, name);
		}
		catch (ParentScopeAbsentException e)
		{
			//accessing 'root' references which are stored not in 'Entity Store', but in Ant project
			if (e.parentLevel - e.scopeLevel == 1)
			{
				String _name = EntityManager.removeParentScopePrefix(name);
				return project.hasReference(_name);
			}

			throw e;
		}

		return exists || project.hasReference(name);
	}

	public static void removeReference(String name, Project project)
	{
		Object removedRef;
		try
		{
			removedRef = EntityManager.removeEntity(ReferenceEntityType.instance, name);
		}
		catch (ParentScopeAbsentException e)
		{
			//accessing 'root' references which are stored not in 'Entity Store', but in Ant project
			if (e.parentLevel - e.scopeLevel == 1)
			{
				project.addReference(EntityManager.removeParentScopePrefix(name), "");
				return;
			}

			throw e;
		}

		if (removedRef == null && project.hasReference(name))
		{
			project.addReference(name, "");
		}
	}
}
