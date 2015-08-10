package com.anttoolkit.jasperreports.report;

import com.anttoolkit.general.entities.*;

public class DataSourceCollectionEntityType extends EntityType
{
	public static final DataSourceCollectionEntityType instance = new DataSourceCollectionEntityType();

	private DataSourceCollectionEntityType() {}

	@Override
	public String getName()
	{
		return "DATA_SOURCE_COLLECTION";
	}
}
