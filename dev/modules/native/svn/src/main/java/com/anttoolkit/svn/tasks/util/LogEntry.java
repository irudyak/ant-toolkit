package com.anttoolkit.svn.tasks.util;

import java.util.*;

public class LogEntry
{
	private String author;
	private String date;
	private String message;
	private int revision;

	private List<PathEntry> pathEntries;

	public LogEntry(String author, String date, String message, String revision, List<PathEntry> pathEntries)
	{
		this.author = author;
		this.date = parseDateTime(date);
		this.message = message;
		this.revision = Integer.parseInt(revision);
		this.pathEntries = pathEntries;
		Collections.sort(this.pathEntries);

		for (PathEntry entry : this.pathEntries)
		{
			entry.setRevision(this.revision);
		}
	}

	public String getAuthor()
	{
		return author;
	}

	public String getDate()
	{
		return date;
	}

	public String getMessage()
	{
		return message;
	}

	public int getRevision()
	{
		return revision;
	}

	public int getEntriesCount()
	{
		return pathEntries.size();
	}

	public boolean isEmpty()
	{
		return getEntriesCount() == 0;
	}

	public List<PathEntry> getPathEntries()
	{
		return pathEntries;
	}

	public void union(LogEntry logEntry)
	{
		if (getRevision() != logEntry.getRevision())
		{
			throw new IllegalStateException("Failed to union log entries for two different revisions " +
					getRevision() + " and " + logEntry.getRevision());
		}

		if (!getAuthor().equals(logEntry.getAuthor()))
		{
			throw new IllegalStateException("Failed to union log entries for revision " +
					getRevision() + ", for two different authors " +
					getAuthor() + " and " + logEntry.getAuthor());
		}

		List<PathEntry> pathEntries = logEntry.getPathEntries();
		for (PathEntry pathEntry : pathEntries)
		{
			if (!hasPathEntry(pathEntry))
			{
				this.pathEntries.add(pathEntry);
			}
		}

		Collections.sort(this.pathEntries);
	}

	public void removeEntriesNotStartedWithPath(String path)
	{
		if (pathEntries == null || pathEntries.isEmpty())
		{
			return;
		}

		int count = pathEntries.size();
		int i = 0;

		while (i < count)
		{
			PathEntry entry = pathEntries.get(i);
			if (!entry.getPath().startsWith(path))
			{
				pathEntries.remove(entry);
				count--;
				continue;
			}

			i++;
		}

		Collections.sort(pathEntries);
	}

	public void removeEntriesStartedWithPath(String path)
	{
		if (pathEntries == null || pathEntries.isEmpty())
		{
			return;
		}

		int count = pathEntries.size();
		int i = 0;

		while (i < count)
		{
			PathEntry entry = pathEntries.get(i);
			if (entry.getPath().startsWith(path))
			{
				pathEntries.remove(entry);
				count--;
				continue;
			}

			i++;
		}

		Collections.sort(pathEntries);
	}

	public void removeEntriesWithPath(String path)
	{
		if (pathEntries == null || pathEntries.isEmpty())
		{
			return;
		}

		int count = pathEntries.size();
		int i = 0;

		while (i < count)
		{
			PathEntry entry = pathEntries.get(i);
			if (entry.getPath().equals(path))
			{
				pathEntries.remove(entry);
				count--;
				continue;
			}

			i++;
		}

		Collections.sort(pathEntries);
	}

	private boolean hasPathEntry(PathEntry pathEntry)
	{
		for (PathEntry _pathEntry : pathEntries)
		{
			if (_pathEntry.getPath().equals(pathEntry.getPath()) &&
				_pathEntry.getAction().equals(pathEntry.getAction()))
			{
				return true;
			}
		}

		return false;
	}

	private String parseDateTime(String date)
	{
		String[] dateTimeParts = date.split("T", -1);
		if (dateTimeParts.length != 2)
		{
			return date;
		}

		String datePart = parseDate(dateTimeParts[0]);
		String timePart = parseTime(dateTimeParts[1]);

		if (datePart == null || timePart == null)
		{
			return date;
		}

		return datePart + " " + timePart;
	}

	private String parseDate(String date)
	{
		String[] parts = date.split("-", -1);
		if (parts.length != 3)
		{
			return null;
		}

		return parts[2] + "." + parts[1] + "." + parts[0];
	}

	private String parseTime(String time)
	{
		String[] parts = time.split(":", -1);
		if (parts.length != 3)
		{
			return null;
		}

		return time.replace("Z", "");
	}
}
