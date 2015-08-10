package com.anttoolkit.jasperreports.tasks;

import java.io.*;

import org.apache.tools.ant.*;

import net.sf.jasperreports.engine.*;

import com.anttoolkit.general.tasks.*;

public class CompileReportTask extends GenericTask
{
	private String src;
	private String dest;

	public void setSrc(String src)
	{
		this.src = src;
	}

	public void setDest(String dest)
	{
		this.dest = dest;
	}

	@Override
	public void doWork() throws BuildException
	{
		File file = new File(getFileFullPath(dest));
		if (file.exists())
		{
			try
			{
				file.delete();
			}
			catch (Throwable e) {}
		}

		try
		{
			JasperCompileManager.compileReportToFile(getFileFullPath(src), getFileFullPath(dest));
		}
		catch (JRException e)
		{
			throw new BuildException("Failed to compile report template: " + src, e);
		}
	}

	@Override
	protected void validate()
	{
		if (src == null || src.trim().isEmpty())
		{
			throw new BuildException("Source jasper report jrmxl template file should be specified");
		}

		File file = new File(getFileFullPath(src));
		if (!file.exists() || file.isDirectory())
		{
			throw new BuildException("Incorrect jasper report jrmxl template file specified");
		}

		if (dest == null || dest.trim().isEmpty())
		{
			throw new BuildException("Destination file for compiled report should be specified");
		}
	}
}
