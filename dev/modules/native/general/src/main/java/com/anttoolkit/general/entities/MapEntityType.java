package com.anttoolkit.general.entities;

public class MapEntityType extends EntityType
{
	public static final MapEntityType instance = new MapEntityType();

	private MapEntityType() {}

	@Override
	public String getName()
	{
		return "MAP";
	}
}
