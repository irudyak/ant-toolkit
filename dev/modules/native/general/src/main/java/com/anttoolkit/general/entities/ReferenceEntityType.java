package com.anttoolkit.general.entities;

public class ReferenceEntityType extends EntityType
{
	public static final ReferenceEntityType instance = new ReferenceEntityType();

	private ReferenceEntityType() {}

	@Override
	public String getName()
	{
		return "REFERENCE";
	}

	@Override
	public boolean allowedForRootScope()
	{
		return false;
	}
}
