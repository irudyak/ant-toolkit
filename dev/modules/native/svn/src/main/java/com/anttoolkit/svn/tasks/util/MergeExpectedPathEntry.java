package com.anttoolkit.svn.tasks.util;

import java.util.*;

public class MergeExpectedPathEntry
{
	private String path;
	private int allowedRevision = -1;
	private String[] allowedActions;
	private String pathSuffixIgnoredInComparison;

	public MergeExpectedPathEntry(String path, String pathSuffixIgnoredInComparison, int allowedRevision, String[] allowedActions)
	{
		this.path = path;
		this.pathSuffixIgnoredInComparison = pathSuffixIgnoredInComparison;
		this.allowedRevision = allowedRevision;
		this.allowedActions = allowedActions;
	}

	public int getAllowedRevision()
	{
		return allowedRevision;
	}

	public boolean isExpectedPathEntry(PathEntry pathEntry)
	{
		String path = pathEntry.getPath();
		if (pathSuffixIgnoredInComparison != null && path.startsWith(pathSuffixIgnoredInComparison))
		{
			path = path.substring(pathSuffixIgnoredInComparison.length());
		}

		if (!this.path.equals(path))
		{
			return false;
		}

		if (allowedRevision > pathEntry.getRevision())
		{
			return false;
		}

		for (String action : allowedActions)
		{
			if (action.equals(pathEntry.getAction()))
			{
				return true;
			}
		}

		return false;
	}

	public boolean isExpectedPathEntryExists(List<PathEntry> pathEntries)
	{
		for (PathEntry pathEntry : pathEntries)
		{
			if (isExpectedPathEntry(pathEntry))
			{
				return true;
			}
		}

		return false;
	}
}
