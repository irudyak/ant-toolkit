package com.anttoolkit.hbase.tasks.data.util;

import com.anttoolkit.general.entities.*;

public class HBaseEntityTypesProvider
		implements EntityTypesProvider
{
	@Override
	public EntityType[] supportedTypes()
	{
		return new EntityType[] {BatchEntityType.instance};
	}
}
