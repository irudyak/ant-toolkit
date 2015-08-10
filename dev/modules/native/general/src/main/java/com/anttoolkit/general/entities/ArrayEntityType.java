package com.anttoolkit.general.entities;

public class ArrayEntityType extends EntityType
{
	public static final ArrayEntityType instance = new ArrayEntityType();

	private ArrayEntityType() {}

	@Override
	public String getName()
	{
		return "ARRAY";
	}
}
