package com.anttoolkit.svn.tasks;

import java.io.*;
import java.util.*;
import java.util.regex.*;

import com.anttoolkit.general.tasks.GenericTask;
import org.apache.tools.ant.*;

import com.anttoolkit.svn.tasks.util.*;

public class IssueReportTask
		extends GenericTask
{
	private String logFile;
	private String reportFile;
	private String issueRegExp;
	private String csvSeparator = ",";

	public void setLogFile(String file)
	{
		logFile = file;
	}

	public void setReportFile(String file)
	{
		reportFile = file;
	}

	public void setIssueRegExp(String regExp)
	{
		issueRegExp = regExp;
	}

	public void setCsvSeparator(String separator)
	{
		csvSeparator = separator;
	}

	@Override
	public void doWork() throws BuildException
	{
		Matcher matcher = Pattern.compile(issueRegExp).matcher("");

		LogParser parser = new LogParser();
		LogReport report = parser.parseLog(getFileFullPath(logFile));

		List<LogEntry> entries = report.getOrderedLogEntries();

		Map<String, LogEntry> issues = new HashMap<String, LogEntry>();

		for (LogEntry entry : entries)
		{
			matcher.reset(entry.getMessage());

			while (matcher.find())
			{
				String issueId = entry.getMessage().substring(matcher.start(), matcher.end());

				LogEntry processedEntry = issues.get(issueId);
				if (processedEntry == null || entry.getRevision() > processedEntry.getRevision())
				{
					issues.put(issueId, entry);
				}
			}
		}

		writeReportToFile(issues);
	}

	protected void validate()
	{
		if (reportFile == null)
		{
			throw new BuildException("Report file is not specified");
		}

		if (logFile == null)
		{
			throw new BuildException("SVN xml log file is not specified");
		}

		if (issueRegExp == null)
		{
			throw new BuildException("Issue regular expression is not specified");
		}
	}

	private void writeReportToFile(Map<String, LogEntry> issues)
	{
		PrintWriter out = null;
		try
		{
			out = new PrintWriter(new OutputStreamWriter(new FileOutputStream(getFileFullPath(reportFile), false), "UTF-8"));

			for (String issueId : issues.keySet())
			{
				LogEntry entry = issues.get(issueId);
				out.println(issueId + csvSeparator + entry.getAuthor() + csvSeparator + entry.getRevision() + csvSeparator + entry.getDate());
			}
		}
		catch (UnsupportedEncodingException e)
		{
			throw new BuildException("Failed to create temporary file \"" + reportFile +
					"\" with UTF-8 encoding", e);
		}
		catch (FileNotFoundException e)
		{
			throw new BuildException("Failed to create temporary file \"" + reportFile +
					"\" for report", e);
		}
		finally
		{
			if (out != null)
			{
				out.close();
			}
		}

	}
}
