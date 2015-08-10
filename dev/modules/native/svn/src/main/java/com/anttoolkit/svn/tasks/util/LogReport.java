package com.anttoolkit.svn.tasks.util;

import java.io.*;
import java.text.*;
import java.util.*;

import org.apache.tools.ant.*;
import org.apache.tools.ant.taskdefs.*;

public class LogReport
{
	private static final String XML_HEADER = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>";
	private static final String REPORT_START_NODE = "<report>";
	private static final String REPORT_END_NODE = "</report>";
	private static final String SUMMARY_START_NODE = "\t<summary>";
	private static final String SUMMARY_END_NODE = "\t</summary>";
	private static final String CHANGES_START_NODE = "\t\t<changes count=\"{0}\">";
	private static final String CHANGES_END_NODE = "\t\t</changes>";
	private static final String AUTHOR_SUMMARY_NODE = "\t\t\t<author name=\"{0}\" count=\"{1}\" percent=\"{2}\"/>";
	private static final String AUTHOR_START_NODE = "\t<author name=\"{0}\" changes=\"{1}\" commits=\"{2}\">";
	private static final String AUTHOR_END_NODE = "\t</author>";
	private static final String LOGENTRY_START_NODE = "\t\t<logentry text=\"{0}\" revision=\"{1}\" date=\"{2}\">";
	private static final String LOGENTRY_END_NODE = "\t\t</logentry>";
	private static final String PATH_NODE = "\t\t\t<path action=\"{0}\">{1}</path>";

	private static final String QUOTE_CHARACTER = "\"";
	private static final String QUOTE_SPECIAL_CHARACTER = "&quot;";

	private static final String ANGLE_OPENING_BRACKET_CHARACTER = "<";
	private static final String ANGLE_OPENING_BRACKET_SPECIAL_CHARACTER = "&lt;";

	private static final String ANGLE_CLOSING_BRACKET_CHARACTER = ">";
	private static final String ANGLE_CLOSING_BRACKET_SPECIAL_CHARACTER = "&gt;";

	private List<LogEntry> logEntries = new LinkedList<LogEntry>();
	private boolean isOrdered = false;

	public void addLogEntry(LogEntry logEntry)
	{
		logEntries.add(logEntry);
		isOrdered = false;
	}

	public int getLogEntriesCount()
	{
		int count = 0;

		for (LogEntry logEntry : logEntries)
		{
			count += logEntry.getEntriesCount();
		}

		return count;
	}

	public int getMinLogEntryRevision()
	{
		if (isEmpty())
		{
			return -1;
		}

		List<LogEntry> logEntries = getOrderedLogEntries();

		return logEntries.get(0).getRevision();
	}

	public int getMaxLogEntryRevision()
	{
		if (isEmpty())
		{
			return -1;
		}

		List<LogEntry> logEntries = getOrderedLogEntries();

		return logEntries.get(logEntries.size() - 1).getRevision();
	}

	public boolean isEmpty()
	{
		return getLogEntriesCount() == 0;
	}

	public List<String> getAuthors()
	{
		List<String> authors = new LinkedList<String>();

		for (LogEntry logEntry : logEntries)
		{
			if (!authors.contains(logEntry.getAuthor()))
			{
				authors.add(logEntry.getAuthor());
			}
		}

		Collections.sort(authors);

		return authors;
	}

	public List<LogEntry> getOrderedLogEntries()
	{
		if (!isOrdered)
		{
			Collections.sort(logEntries, LogEntryComparator.instance());
			isOrdered = true;
		}

		return logEntries;
	}

	public List<LogEntry> getOrderedAuthorLogEntries(String author)
	{
		List<LogEntry> entries = getUnorderedAuthorLogEntries(author);
		Collections.sort(entries, LogEntryComparator.instance());

		return entries;
	}

	public List<String> getAuthorUniquePaths(String author)
	{
		List<LogEntry> logEntries = getUnorderedAuthorLogEntries(author);
		List<String> uniquePathEntries = new LinkedList<String>();

		for (LogEntry logEntry : logEntries)
		{
			List<PathEntry> pathEntries = logEntry.getPathEntries();
			for (PathEntry pathEntry : pathEntries)
			{
				if (!uniquePathEntries.contains(pathEntry.getPath()))
				{
					uniquePathEntries.add(pathEntry.getPath());
				}
			}
		}

		return uniquePathEntries;
	}

	public List<String> getUniquePaths()
	{
		List<String> uniquePathEntries = new LinkedList<String>();

		for (LogEntry logEntry : logEntries)
		{
			List<PathEntry> pathEntries = logEntry.getPathEntries();
			for (PathEntry pathEntry : pathEntries)
			{
				if (!uniquePathEntries.contains(pathEntry.getPath()))
				{
					uniquePathEntries.add(pathEntry.getPath());
				}
			}
		}

		return uniquePathEntries;
	}

	public List<PathEntry> getOrderedByRevisionAuthorPathEntries(String author)
	{
		List<PathEntry> orderedPathEntries = new LinkedList<PathEntry>();

		List<LogEntry> logEntries = getUnorderedAuthorLogEntries(author);
		for (LogEntry logEntry : logEntries)
		{
			List<PathEntry> pathEntries = logEntry.getPathEntries();
			for (PathEntry pathEntry : pathEntries)
			{
				orderedPathEntries.add(pathEntry);
			}
		}

		Collections.sort(orderedPathEntries);

		return orderedPathEntries;
	}

	public boolean hasSamePathEntryWithHigherRevisionForAuthor(PathEntry pathEntry, String author)
	{
		for (LogEntry logEntry : logEntries)
		{
			if (!logEntry.getAuthor().equals(author) || logEntry.getRevision() < pathEntry.getRevision())
			{
				continue;
			}

			List<PathEntry> pathEntries = logEntry.getPathEntries();
			for (PathEntry _pathEntry : pathEntries)
			{
				if (_pathEntry.getPath().equals(pathEntry.getPath()) &&
					_pathEntry.getAction().equals(pathEntry.getAction()))
				{
					return true;
				}
			}
		}

		return false;
	}

	public int getLastModificationRevisionForPath(String path)
	{
		int revision = -1;

		for (LogEntry logEntry : logEntries)
		{
			List<PathEntry> pathEntries = logEntry.getPathEntries();
			for (PathEntry pathEntry : pathEntries)
			{
				if (pathEntry.getPath().equals(path) && revision < pathEntry.getRevision())
				{
					revision = pathEntry.getRevision();
					break;
				}
			}
		}

		return revision;
	}

	public int getAuthorLogEntriesCount(String author)
	{
		List<LogEntry> logEntries = getUnorderedAuthorLogEntries(author);

		int count = 0;

		for (LogEntry logEntry : logEntries)
		{
			count += logEntry.getEntriesCount();
		}

		return count;
	}

	public int getAuthorLogEntriesPercent(String author)
	{
		return (getAuthorLogEntriesCount(author) * 100) / getLogEntriesCount();
	}

	public void union(LogReport report)
	{
		if (report == null)
		{
			return;
		}

		List<LogEntry> logEntries = report.getOrderedLogEntries();
		for (LogEntry logEntry : logEntries)
		{
			LogEntry _logEntry = getLogEntryByRevision(logEntry.getRevision());
			if (_logEntry == null)
			{
				this.addLogEntry(logEntry);
				continue;
			}

			_logEntry.union(logEntry);
		}

		isOrdered = false;
	}

	public LogReport getLogReportForAuthor(String author)
	{
		LogReport report = new LogReport();

		for (LogEntry entry : logEntries)
		{
			if (entry.getAuthor().equals(author))
			{
				report.addLogEntry(entry);
			}
		}

		return report.isEmpty() ? null : report;
	}

	public LogEntry getLogEntryByRevision(int revision)
	{
		for (LogEntry logEntry : logEntries)
		{
			if (logEntry.getRevision() == revision)
			{
				return logEntry;
			}
		}

		return null;
	}

	public void removeEntriesNotStartedWithPath(String path)
	{
		if (logEntries == null || logEntries.isEmpty())
		{
			return;
		}

		int count = logEntries.size();
		int i = 0;

		while (i < count)
		{
			LogEntry logEntry = logEntries.get(i);
			logEntry.removeEntriesNotStartedWithPath(path);

			if (logEntry.isEmpty())
			{
				logEntries.remove(logEntry);
				count--;
				continue;
			}

			i++;
		}

		isOrdered = false;
	}

	public void removeEntriesStartedWithPath(String path)
	{
		if (logEntries == null || logEntries.isEmpty())
		{
			return;
		}

		int count = logEntries.size();
		int i = 0;

		while (i < count)
		{
			LogEntry logEntry = logEntries.get(i);
			logEntry.removeEntriesStartedWithPath(path);

			if (logEntry.isEmpty())
			{
				logEntries.remove(logEntry);
				count--;
				continue;
			}

			i++;
		}

		isOrdered = false;
	}

	public void removeEntriesWithPath(String path)
	{
		if (logEntries == null || logEntries.isEmpty())
		{
			return;
		}

		int count = logEntries.size();
		int i = 0;

		while (i < count)
		{
			LogEntry logEntry = logEntries.get(i);
			logEntry.removeEntriesWithPath(path);

			if (logEntry.isEmpty())
			{
				logEntries.remove(logEntry);
				count--;
				continue;
			}

			i++;
		}

		isOrdered = false;
	}

	public void removeEntriesWithPathForAuthor(String path, String author)
	{
		if (logEntries == null || logEntries.isEmpty())
		{
			return;
		}

		int count = logEntries.size();
		int i = 0;

		while (i < count)
		{
			LogEntry logEntry = logEntries.get(i);
			if (!logEntry.getAuthor().equals(author))
			{
				i++;
				continue;
			}

			logEntry.removeEntriesWithPath(path);
			if (logEntry.isEmpty())
			{
				logEntries.remove(logEntry);
				count--;
				continue;
			}

			i++;
		}

		isOrdered = false;
	}

	public void removeLogEntriesWithRevisionLowerThan(int revision)
	{
		List<LogEntry> logEntries = getOrderedLogEntries();
		if (logEntries.isEmpty())
		{
			return;
		}

		int count = this.logEntries.size();
		int i = 0;

		while (i < count)
		{
			LogEntry entry = this.logEntries.get(i);
			if (entry.getRevision() < revision)
			{
				this.logEntries.remove(entry);
				count--;
				continue;
			}

			i++;
		}

		isOrdered = false;
	}

	public void removeNotLastModifications()
	{
		List<String> uniquePaths = getUniquePaths();
		for (String path : uniquePaths)
		{
			int lastRevision = getLastModificationRevisionForPath(path);

			int count = logEntries.size();
			int i = 0;

			while (i < count)
			{
				LogEntry entry = logEntries.get(i);

				if (entry.getRevision() < lastRevision)
				{
					entry.removeEntriesWithPath(path);
					if (entry.isEmpty())
					{
						logEntries.remove(entry);
						count--;
						i--;
					}
				}

				i++;
			}
		}

		isOrdered = false;
	}

	public void writeReport(String file, XSLTProcess xsltTask)
	{
		String tempFile = file + "__";

		PrintWriter out = null;
		try
		{
			out = new PrintWriter(new OutputStreamWriter(new FileOutputStream(tempFile, false), "UTF-8"));
			writeReport(out);
		}
		catch (UnsupportedEncodingException e)
		{
			throw new BuildException("Failed to create temporary file \"" + tempFile +
					"\" with UTF-8 encoding", e);
		}
		catch (FileNotFoundException e)
		{
			throw new BuildException("Failed to create temporary file \"" + tempFile +
					"\" for report", e);
		}
		finally
		{
			if (out != null)
			{
				out.close();
			}
		}

		xsltTask.setIn(new File(tempFile));
		xsltTask.setOut(new File(file));
		xsltTask.perform();

		(new File(tempFile)).delete();
	}

	public boolean isExpectedPathEntryExists(MergeExpectedPathEntry expectedEntry)
	{
		if (expectedEntry == null)
		{
			return false;
		}

		for (LogEntry logEntry : logEntries)
		{
			if (logEntry.getRevision() < expectedEntry.getAllowedRevision())
			{
				continue;
			}

			if (expectedEntry.isExpectedPathEntryExists(logEntry.getPathEntries()))
			{
				return true;
			}
		}

		return false;
	}

	private void writeReport(PrintWriter out)
	{
		out.println(XML_HEADER);
		out.println(REPORT_START_NODE);

		writeSummary(out);

		writeChanges(out);

		out.println(REPORT_END_NODE);
	}

	private void writeSummary(PrintWriter out)
	{
		out.println(SUMMARY_START_NODE);
		out.println(MessageFormat.format(CHANGES_START_NODE, new String[]{Integer.toString(getLogEntriesCount())}));

		List<String> authors = getAuthors();
		for (String author : authors)
		{
			out.println(MessageFormat.format(AUTHOR_SUMMARY_NODE,
					new String[]{author, Integer.toString(getAuthorLogEntriesCount(author)),
							Integer.toString(getAuthorLogEntriesPercent(author))}));
		}

		out.println(CHANGES_END_NODE);
		out.println(SUMMARY_END_NODE);
	}

	private void writeChanges(PrintWriter out)
	{
		List<String> authors = getAuthors();
		for (String author : authors)
		{
			List<LogEntry> logEntries = getOrderedAuthorLogEntries(author);
			if (logEntries == null || logEntries.isEmpty())
			{
				continue;
			}

			out.println(MessageFormat.format(AUTHOR_START_NODE,
					new String[]{author, Integer.toString(getAuthorLogEntriesCount(author)), Integer.toString(logEntries.size())}));

			for (LogEntry logEntry : logEntries)
			{
				writeLogEntry(out, logEntry);
			}

			out.println(AUTHOR_END_NODE);
		}
	}

	private void writeLogEntry(PrintWriter out, LogEntry logEntry)
	{
		if (logEntry.getEntriesCount() == 0)
		{
			return;
		}

		String message = logEntry.getMessage().replace(QUOTE_CHARACTER, QUOTE_SPECIAL_CHARACTER);
		message = message.replace(ANGLE_OPENING_BRACKET_CHARACTER, ANGLE_OPENING_BRACKET_SPECIAL_CHARACTER);
		message = message.replace(ANGLE_CLOSING_BRACKET_CHARACTER, ANGLE_CLOSING_BRACKET_SPECIAL_CHARACTER);

		out.println(MessageFormat.format(LOGENTRY_START_NODE,
				new String[]{message, Integer.toString(logEntry.getRevision()), logEntry.getDate()}));

		List<PathEntry> pathEntries = logEntry.getPathEntries();
		for (PathEntry pathEntry : pathEntries)
		{
			out.println(MessageFormat.format(PATH_NODE,
					new String[]{pathEntry.getAction(), pathEntry.getPath()}));
		}

		out.println(LOGENTRY_END_NODE);
	}

	private List<LogEntry> getUnorderedAuthorLogEntries(String author)
	{
		List<LogEntry> entries = new LinkedList<LogEntry>();

		for (LogEntry logEntry : logEntries)
		{
			if (logEntry.getAuthor().equals(author))
			{
				entries.add(logEntry);
			}
		}

		return entries;
	}
}
