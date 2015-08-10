package com.anttoolkit.jasperreports.report;

import com.anttoolkit.general.entities.*;

public class JasperReportEntityType extends EntityType
{
	public static final JasperReportEntityType instance = new JasperReportEntityType();

	private JasperReportEntityType() {}

	@Override
	public String getName()
	{
		return "JASPER_REPORT";
	}

	public boolean allowedForLocalScope()
	{
		return false;
	}

	public boolean allowedForRootScope()
	{
		return false;
	}

	public boolean allowedForGlobalScope()
	{
		return true;
	}
}
