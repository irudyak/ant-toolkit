package com.anttoolkit.svn.tasks;

import com.anttoolkit.general.tasks.GenericTask;
import com.anttoolkit.svn.tasks.util.*;

import org.apache.tools.ant.*;
import org.apache.tools.ant.taskdefs.*;

public class LogReportTask
		extends GenericTask
{
	private String logFile;
	private String reportFile;
	private String xsltStyle;

	public void setLogFile(String file)
	{
		logFile = file;
	}

	public void setReportFile(String file)
	{
		reportFile = file;
	}

	public void setXsltStyle(String file)
	{
		xsltStyle = file;
	}

	public void doWork()
		throws BuildException
	{
		LogParser parser = new LogParser();
		LogReport report = parser.parseLog(getFileFullPath(logFile));

		XSLTProcess xsltTask = new XSLTProcess();
		this.initDelegateTask(xsltTask);
		xsltTask.setStyle(xsltStyle);

		report.writeReport(reportFile, xsltTask);
	}

	protected void validate()
	{
		if (xsltStyle == null)
		{
			throw new BuildException("XSLT style sheet is not specified");
		}

		if (reportFile == null)
		{
			throw new BuildException("Report file is not specified");
		}

		if (logFile == null)
		{
			throw new BuildException("SVN xml log file is not specified");
		}
	}
}
