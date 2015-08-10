package com.anttoolkit.svn.tasks.util;

import java.util.*;

public class LogEntryComparator implements Comparator<LogEntry>
{
	private static final LogEntryComparator instance = new LogEntryComparator();

	public static LogEntryComparator instance()
	{
		return instance;
	}

	private LogEntryComparator() {}

	public int compare(LogEntry entry1, LogEntry entry2)
	{
		return entry1.getRevision() < entry2.getRevision() ? -1 :
				entry1.getRevision() > entry2.getRevision() ? 1 : 0;
	}
}
