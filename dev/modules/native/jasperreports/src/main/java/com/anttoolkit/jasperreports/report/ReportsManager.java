package com.anttoolkit.jasperreports.report;

import org.apache.tools.ant.*;

import net.sf.jasperreports.engine.*;

import com.anttoolkit.general.entities.*;

public class ReportsManager
{
	private static final IEntityProcessor ENTITY_PROCESSOR = new IEntityProcessor<JasperReport, IReportProcessor, JasperPrint> ()
	{
		@Override
		public JasperPrint processEntity(JasperReport report, IReportProcessor processor)
		{
			return processor.fillReport(report);
		}

		@Override
		public boolean readOnly()
		{
			return true;
		}
	};

	public static JasperPrint fillReport(String cacheKey, IReportProcessor processor)
	{
		try
		{
			return (JasperPrint)EntityManager.processEntity(JasperReportEntityType.instance, EntityManager.GLOBAL_SCOPE_PREFIX + cacheKey, ENTITY_PROCESSOR, processor);
		}
		catch (EntityNotFoundException e)
		{
			return null;
		}
	}

	public static void cacheReport(String cacheKey, JasperReport report)
	{
		try
		{
			EntityManager.setEntity(JasperReportEntityType.instance, EntityManager.GLOBAL_SCOPE_PREFIX + cacheKey, report, false);
		}
		catch (EntityStorageAbsentException e)
		{
			throw new BuildException("There are no storage registered to cache Jasper report", e);
		}
	}

	public static boolean wasReportCached(String cacheKey)
	{
		return EntityManager.exists(JasperReportEntityType.instance, EntityManager.GLOBAL_SCOPE_PREFIX + cacheKey);
	}
}
