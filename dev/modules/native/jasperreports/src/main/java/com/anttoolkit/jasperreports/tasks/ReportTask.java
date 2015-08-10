package com.anttoolkit.jasperreports.tasks;

import java.io.*;
import java.util.*;

import org.apache.tools.ant.*;

import net.sf.jasperreports.engine.*;
import net.sf.jasperreports.engine.data.*;
import net.sf.jasperreports.engine.export.*;

import com.anttoolkit.jasperreports.report.*;
import com.anttoolkit.sql.common.*;
import com.anttoolkit.general.tasks.*;

public class ReportTask
		extends GenericTask
		implements IDataSourceProcessor<JRDataSource>, IReportProcessor
{
	private static final String UNCOMPILED_TEMPLATE_EXT = "jrxml";

	//report formats
	private static final String REPORT_FORMAT_PDF = "pdf";
	private static final String REPORT_FORMAT_RTF = "rtf";
	private static final String REPORT_FORMAT_EXCEL = "excel";
	private static final String REPORT_FORMAT_HTML = "html";
	private static final String REPORT_FORMAT_XML = "xml";
	private static final String REPORT_FORMAT_CSV = "csv";
	private static final String REPORT_FORMAT_PLAIN_TEXT = "text";

	private String template;
	private String format;
	private String reportFile;
	private String dataSource;
	private List<ReportParam> params = new LinkedList<ReportParam>();

	public void setTemplate(String template)
	{
		this.template = template;
	}

	public void setFormat(String format)
	{
		if (format == null)
		{
			throw new BuildException("Format can't be null");
		}

		this.format = format.trim().toLowerCase();
	}

	public void setReportFile(String file)
	{
		reportFile = file;
	}

	public void setDataSource(String dataSource)
	{
		this.dataSource = dataSource;
	}

	public void addConfiguredParam(ReportParam param)
	{
		params.add(param);
	}

	@Override
	public void doWork() throws BuildException
	{
		FileOutputStream out = null;

		try
		{
			out = new FileOutputStream(prepareReportFile());

			JRAbstractExporter exporter = getReportExporter();
			exporter.setParameter(JRExporterParameter.JASPER_PRINT, generateReport());
			exporter.setParameter(JRExporterParameter.OUTPUT_STREAM, out);

			exporter.exportReport();
		}
		catch (JRException e)
		{
			throw new BuildException("Failed to export generated report to file: " + getFileFullPath(reportFile), e);
		}
		catch (FileNotFoundException e)
		{
			throw new BuildException("Failed to locate file '" + getFileFullPath(reportFile) + "' for the new report");
		}
		finally
		{
			if (out != null)
			{
				try
				{
					out.close();
				}
				catch (Throwable e) {}
			}
		}
	}

	@Override
	public JRDataSource processDataSource(Collection<?> coll)
	{
		Iterator<?> iter = coll.iterator();
		Object firstObj = !iter.hasNext() ? null : iter.next();

		return firstObj != null && firstObj instanceof Map ?
				new JRMapCollectionDataSource((Collection<Map<String,?>>)coll) :
				new JRBeanCollectionDataSource(coll);
	}

	@Override
	public JasperPrint fillReport(JasperReport report)
	{
		try
		{
			if (dataSource != null)
			{
				return JasperFillManager.fillReport(report, getReportParams(), (JRDataSource)DataSourceManager.processDataSource(dataSource, this));
			}

			if (SqlSessionManager.hasSession())
			{
				return JasperFillManager.fillReport(report, getReportParams(), SqlSessionManager.getSession().getConnection());
			}

			return JasperFillManager.fillReport(report, getReportParams(), new JREmptyDataSource());
		}
		catch (JRException e)
		{
			throw new BuildException("Error occured while trying to generate report", e);
		}
	}

	@Override
	protected void validate()
	{
		if (template == null || template.trim().isEmpty())
		{
			throw new BuildException("Report template should be specified");
		}

		File file = new File(getFileFullPath(template));
		if (!file.exists() || file.isDirectory())
		{
			throw new BuildException("Incorrect report template file specified: " + template);
		}

		if (format == null || format.trim().isEmpty())
		{
			throw new BuildException("Report format should be specified");
		}

		if (!REPORT_FORMAT_PDF.equals(format) && !REPORT_FORMAT_RTF.equals(format) &&
			!REPORT_FORMAT_EXCEL.equals(format) && !REPORT_FORMAT_HTML.equals(format) &&
			!REPORT_FORMAT_XML.equals(format) && !REPORT_FORMAT_CSV.equals(format) &&
			!REPORT_FORMAT_PLAIN_TEXT.equals(format))
		{
			throw new BuildException("Incorrect report format specified");
		}
/*
		if (dataSource == null && !SqlSessionManager.hasSession())
		{
			throw new BuildException("To be able to build jasper report either DataSource should be specified or " +
					"report task should be included into SQL tasks container, to be able to obtain SQL session");
		}
*/
		if (reportFile == null)
		{
			throw new BuildException("Report file should be specified");
		}
	}

	private Map<String, Object> getReportParams()
	{
		Map<String, Object> reportParams = new HashMap<String, Object>();

		for (ReportParam param : params)
		{
			reportParams.put(param.getName(), param.getValue());
		}

		return reportParams;
	}

	private JRAbstractExporter getReportExporter()
	{
		if (REPORT_FORMAT_PDF.equalsIgnoreCase(format))
		{
			return new JRPdfExporter();
		}

		if (REPORT_FORMAT_RTF.equalsIgnoreCase(format))
		{
			return new JRRtfExporter();
		}

		if (REPORT_FORMAT_EXCEL.equalsIgnoreCase(format))
		{
			return new JExcelApiExporter();
		}

		if (REPORT_FORMAT_HTML.equalsIgnoreCase(format))
		{
			return new JRHtmlExporter();
		}

		if (REPORT_FORMAT_XML.equalsIgnoreCase(format))
		{
			return new JRXmlExporter();
		}

		if (REPORT_FORMAT_CSV.equalsIgnoreCase(format))
		{
			return new JRCsvExporter();
		}

		if (REPORT_FORMAT_PLAIN_TEXT.equalsIgnoreCase(format))
		{
			return new JRTextExporter();
		}

		throw new IllegalArgumentException("Incorrect report format '" + format + "' specified");
	}

	private File prepareReportFile()
	{
		String fullPath = getFileFullPath(reportFile);
		File file = new File(getFileFullPath(fullPath));

		//remove existing file
		if (file.exists())
		{
			try
			{
				if (!file.delete())
				{
					throw new BuildException("Failed to delete existing report file '" + fullPath + "' to generate new report");
				}
			}
			catch (Throwable e)
			{
				throw new BuildException("Failed to delete existing report file '" + fullPath + "' to generate new report", e);
			}
		}

		//create new file
		try
		{
			if (!file.createNewFile())
			{
				throw new BuildException("Failed to create new file '" + fullPath + "' to generate report into it");
			}
		}
		catch (IOException e)
		{
			throw new BuildException("Failed to create new file '" + fullPath + "' to generate report into it", e);
		}

		return file;
	}

	private JasperPrint generateReport()
	{
		String reportTemplate = getFileFullPath(template);

		try
		{
			if (!template.toLowerCase().endsWith(UNCOMPILED_TEMPLATE_EXT))
			{
				if (dataSource != null)
				{
					return JasperFillManager.fillReport(reportTemplate, getReportParams(), (JRDataSource)DataSourceManager.processDataSource(dataSource, this));
				}

				if (SqlSessionManager.hasSession())
				{
					return JasperFillManager.fillReport(reportTemplate, getReportParams(), SqlSessionManager.getSession().getConnection());
				}

				return JasperFillManager.fillReport(reportTemplate, getReportParams(), new JREmptyDataSource());
			}

			String cacheKey = Integer.toString(reportTemplate.hashCode());

			if (ReportsManager.wasReportCached(cacheKey))
			{
				JasperPrint print = ReportsManager.fillReport(cacheKey, this);
				if (print != null)
				{
					return print;
				}
			}

			JasperReport report = JasperCompileManager.compileReport(reportTemplate);
			ReportsManager.cacheReport(cacheKey, report);

			if (dataSource != null)
			{
				return JasperFillManager.fillReport(report, getReportParams(), (JRDataSource)DataSourceManager.processDataSource(dataSource, this));
			}

			if (SqlSessionManager.hasSession())
			{
				return JasperFillManager.fillReport(report, getReportParams(), SqlSessionManager.getSession().getConnection());
			}

			return JasperFillManager.fillReport(report, getReportParams(), new JREmptyDataSource());
		}
		catch (JRException e)
		{
			throw new BuildException("Error occured while trying to generate report", e);
		}
	}
}
