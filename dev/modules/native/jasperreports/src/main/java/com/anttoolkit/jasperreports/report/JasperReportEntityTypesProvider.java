package com.anttoolkit.jasperreports.report;

import com.anttoolkit.general.entities.*;

public class JasperReportEntityTypesProvider
		implements EntityTypesProvider
{
	@Override
	public EntityType[] supportedTypes()
	{
		return new EntityType[] {DataSourceCollectionEntityType.instance, JasperReportEntityType.instance};
	}
}
