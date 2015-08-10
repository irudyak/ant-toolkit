package com.anttoolkit.general.entities;

public abstract class EntityType
{
	public abstract String getName();

	public boolean allowedForLocalScope()
	{
		return true;
	}

	public boolean allowedForRootScope()
	{
		return true;
	}

	public boolean allowedForGlobalScope()
	{
		return true;
	}

	@Override
	public boolean equals(Object obj)
	{
		return obj != null && obj instanceof EntityType && getName().equals(((EntityType)obj).getName());
	}

	@Override
	public int hashCode()
	{
		return getName().hashCode();
	}

	@Override
	public String toString()
	{
		return getName();
	}
}
