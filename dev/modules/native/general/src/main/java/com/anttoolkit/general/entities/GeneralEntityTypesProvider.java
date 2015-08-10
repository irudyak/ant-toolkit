package com.anttoolkit.general.entities;

public class GeneralEntityTypesProvider
		implements EntityTypesProvider
{
	@Override
	public EntityType[] supportedTypes()
	{
		return new EntityType[] {PropertyEntityType.instance, ReferenceEntityType.instance, ArrayEntityType.instance, MapEntityType.instance, QueueEntityType.instance};
	}
}
