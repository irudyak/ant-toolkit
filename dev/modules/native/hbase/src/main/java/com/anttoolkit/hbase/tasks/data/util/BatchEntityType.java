package com.anttoolkit.hbase.tasks.data.util;

import com.anttoolkit.general.entities.*;

public class BatchEntityType extends EntityType
{
	public static final BatchEntityType instance = new BatchEntityType();

	private BatchEntityType() {}

	@Override
	public String getName()
	{
		return "HBASE_BATCH";
	}
}
