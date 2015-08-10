package com.anttoolkit.general.props;

import java.text.*;

import org.apache.tools.ant.*;
import org.apache.tools.ant.PropertyHelper.*;
import org.apache.tools.ant.property.*;

import com.anttoolkit.general.entities.*;

public class ScopePropertyHandler
		implements PropertyExpander, PropertyEvaluator, PropertySetter
{
	private static final ScopePropertyHandler INSTANCE = new ScopePropertyHandler();
	private static final IEntityProcessor READER = new PropertyReader();

	@Override
	public Object evaluate(String property, PropertyHelper propertyHelper)
	{
		try
		{
			return EntityManager.processEntity(PropertyEntityType.instance, property, READER, null);
		}
		catch (EntityNotFoundException e)
		{
			return null;
		}
		catch (ParentScopeAbsentException e)
		{
			//accessing 'root' properties which are stored not in 'Entity Store', but in Ant project
			if (e.parentLevel - e.scopeLevel == 1)
			{
				EntityManager.skipEntitiesProcessing(PropertyEntityType.instance);

				try
				{
					return propertyHelper.getProject().getProperty(EntityManager.removeParentScopePrefix(property));
				}
				finally
				{
					EntityManager.resumeEntitiesProcessing(PropertyEntityType.instance);
				}
			}

			throw e;
		}
	}

	@Override
	public boolean setNew(String property, Object value, PropertyHelper propertyHelper)
	{
		if (property == null)
		{
			return false;
		}

		try
		{
			boolean alreadyExist = EntityManager.existsInCPGScopes(PropertyEntityType.instance, property);
			if (!alreadyExist)
			{
				EntityManager.setEntity(PropertyEntityType.instance, property, value, false);
			}

			return true;
		}
		catch (EntityStorageAbsentException e)
		{
			return false;
		}
		catch (ParentScopeAbsentException e)
		{
			//accessing 'root' properties which are stored not in 'Entity Store', but in Ant project
			if (e.parentLevel - e.scopeLevel == 1)
			{
				EntityManager.skipEntitiesProcessing(PropertyEntityType.instance);

				try
				{
					propertyHelper.setNewProperty(EntityManager.removeParentScopePrefix(property), value);
				}
				finally
				{
					EntityManager.resumeEntitiesProcessing(PropertyEntityType.instance);
				}

				return true;
			}

			throw e;
		}
	}

	@Override
	public boolean set(String property, Object value, PropertyHelper propertyHelper)
	{
		if (property == null)
		{
			return false;
		}

		try
		{
			EntityManager.setEntity(PropertyEntityType.instance, property, value, false);
			return true;
		}
		catch (EntityStorageAbsentException e)
		{
			return false;
		}
		catch (ParentScopeAbsentException e)
		{
			//accessing 'root' properties which are stored not in 'Entity Store', but in Ant project
			if (e.parentLevel - e.scopeLevel == 1)
			{
				EntityManager.skipEntitiesProcessing(PropertyEntityType.instance);

				try
				{
					propertyHelper.setProperty(EntityManager.removeParentScopePrefix(property), value, false);
				}
				finally
				{
					EntityManager.resumeEntitiesProcessing(PropertyEntityType.instance);
				}

				return true;
			}

			throw e;
		}
	}

	/**
	* Parse the next property name.
	* @param value the String to parse.
	* @param pos the ParsePosition in use.
	* @param parseNextProperty parse next property
	* @return parsed String if any, else <code>null</code>.
	*/
	public String parsePropertyName(String value, ParsePosition pos, ParseNextProperty parseNextProperty)
	{
		int start = pos.getIndex();

		if (value.length() - start >= 3 && '$' == value.charAt(start) && '{' == value.charAt(start + 1))
		{
			parseNextProperty.getProject().log("Attempting nested property processing", Project.MSG_DEBUG);
			pos.setIndex(start + 2);

			StringBuilder sb = new StringBuilder();

			for (int c = pos.getIndex(); c < value.length(); c = pos.getIndex())
			{
				if (value.charAt(c) == '}')
				{
					pos.setIndex(c + 1);
					return sb.toString();
				}

				Object o = parseNextProperty.parseNextProperty(value, pos);

				if (o != null)
				{
					sb.append(o);
				}
				else
				{
					// be aware that the parse position may now have changed;
					// update:
					c = pos.getIndex();
					sb.append(value.charAt(c));
					pos.setIndex(c + 1);
				}
			}
		}

		pos.setIndex(start);

		return null;
	}

	/**
	 * {@inheritDoc}
	 */
	public boolean equals(Object obj)
	{
		return obj == this || obj instanceof ScopePropertyHandler && obj.hashCode() == hashCode();
	}

	/**
	 * {@inheritDoc}
	 */
	public int hashCode()
	{
		if (ScopePropertyHandler.class.equals(getClass()))
		{
			return System.identityHashCode(INSTANCE);
		}

		throw new UnsupportedOperationException("Get your own hashCode implementation!");
	}
}
