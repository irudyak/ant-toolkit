package com.anttoolkit.jasperreports.report;

import net.sf.jasperreports.engine.*;

public interface IReportProcessor
{
	public JasperPrint fillReport(JasperReport report);
}
