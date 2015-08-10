package com.anttoolkit.general.entities;

public class PropertyEntityType extends EntityType
{
	public static final PropertyEntityType instance = new PropertyEntityType();

	private PropertyEntityType() {}

	@Override
	public String getName()
	{
		return "PROPERTY";
	}

	@Override
	public boolean allowedForRootScope()
	{
		return false;
	}
}
