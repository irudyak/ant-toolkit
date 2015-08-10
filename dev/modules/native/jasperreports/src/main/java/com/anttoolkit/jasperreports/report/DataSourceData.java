package com.anttoolkit.jasperreports.report;

import com.anttoolkit.general.tasks.GenericTask;

public class DataSourceData
{
	private String text;
	private String file;
	private String fileEncoding;
	private String separator = ",";
	private boolean trimValues = true;

	public void resolveProperties(GenericTask task)
	{
		if (text != null)
		{
			text = task.substituteProperties(text);
		}
	}

	public void setSeparator(String separator)
	{
		this.separator = separator;
	}

	public void setFile(String file)
	{
		this.file = file;
	}

	public void setFileEncoding(String encoding)
	{
		fileEncoding = encoding;
	}

	public void addText(String text)
	{
		this.text = text;
	}

	public void setTrimValues(boolean trim)
	{
		trimValues = trim;
	}

	public String getSeparator()
	{
		return separator;
	}

	public String getFile()
	{
		return file;
	}

	public String getFileEncoding()
	{
		return fileEncoding;
	}

	public String getText()
	{
		return text;
	}

	public boolean trimValues()
	{
		return trimValues;
	}
}
