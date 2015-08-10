package com.anttoolkit.svn.tasks.util;

import java.util.*;

public class MergeHelper
{
	public static void removeMutuallyExclusivePathEntriesFromBranchReport(LogReport branchReport)
	{
		if (branchReport == null || branchReport.isEmpty())
		{
			return;
		}

		List<String> authors = branchReport.getAuthors();
		for (String author : authors)
		{
			List<String> uniquePaths = branchReport.getAuthorUniquePaths(author);

			List<PathEntry> orderedPathEntries = branchReport.getOrderedByRevisionAuthorPathEntries(author);

			if (uniquePaths == null || uniquePaths.isEmpty())
			{
				continue;
			}

			for (String path : uniquePaths)
			{
				PathEntry firstEntry = getFirstPathEntryFromOrderedPathEntries(path, orderedPathEntries);
				PathEntry lastEntry = getLastPathEntryFromOrderedPathEntries(path, orderedPathEntries);

				if (firstEntry == null || lastEntry == null || firstEntry.equals(lastEntry))
				{
					continue;
				}

				if ((firstEntry.isAddAction() && lastEntry.isDeleteAction()) ||
					(firstEntry.isDeleteAction() && lastEntry.isAddAction() &&
					!hasActionForPathBetweenRevisions(orderedPathEntries, firstEntry.getRevision(), lastEntry.getRevision(), path, null)))
				{
					branchReport.removeEntriesWithPathForAuthor(path, author);
				}
			}
		}
	}

	public static LogReport getUnmergedChangesReport(LogReport trunkReport, LogReport branchReport, String trunkRoot, String branchRoot, String author)
	{
		if (branchReport == null || branchReport.isEmpty() ||
			branchReport.getMinLogEntryRevision() > trunkReport.getMaxLogEntryRevision())
		{
			return null;
		}

		LogReport authorReport = branchReport.getLogReportForAuthor(author);

		List<String> uniquePaths = authorReport.getAuthorUniquePaths(author);
		List<PathEntry> branchAuthorOrderedPathEntries = authorReport.getOrderedByRevisionAuthorPathEntries(author);

		for (String path : uniquePaths)
		{
			MergeExpectedPathEntry expectedEntry = getAuthorExpectedPathEntryForBranchPath(path, trunkRoot, branchRoot, branchAuthorOrderedPathEntries);
			if (expectedEntry == null || trunkReport.isExpectedPathEntryExists(expectedEntry))
			{
				authorReport.removeEntriesWithPath(path);
			}
		}

		return authorReport;
	}

	private static PathEntry getFirstPathEntryFromOrderedPathEntries(String path, List<PathEntry> orderedPathEntries)
	{
		for (PathEntry pathEntry : orderedPathEntries)
		{
			if (pathEntry.getPath().equals(path))
			{
				return pathEntry;
			}
		}

		return null;
	}

	private static PathEntry getLastPathEntryFromOrderedPathEntries(String path, List<PathEntry> orderedPathEntries)
	{
		PathEntry _pathEntry = null;

		for (PathEntry pathEntry : orderedPathEntries)
		{
			if (pathEntry.getPath().equals(path))
			{
				_pathEntry = pathEntry;
			}
		}

		return _pathEntry;
	}

	private static MergeExpectedPathEntry getAuthorExpectedPathEntryForBranchPath(String path, String trunkRoot, String branchRoot, List<PathEntry> branchAuthorOrderedPathEntries)
	{
		PathEntry lastEntry = PathEntry.getLastOccurenceOfPath(path, branchAuthorOrderedPathEntries);

		String unifiedPath = path.substring(branchRoot.length());

		if (lastEntry.isModifyAction())
		{
			return new MergeExpectedPathEntry(unifiedPath, trunkRoot, lastEntry.getRevision() + 1, new String[] {PathEntry.ADD_ACTION, PathEntry.MODIFY_ACTION});
		}

		return new MergeExpectedPathEntry(unifiedPath, trunkRoot, lastEntry.getRevision() + 1, new String[] {lastEntry.getAction()});
	}


	private static boolean hasActionForPathBetweenRevisions(List<PathEntry> orderedPathEntries, int startRevision, int endRevision, String path, String action)
	{
		for (PathEntry pathEntry : orderedPathEntries)
		{
			if (pathEntry.getRevision() < startRevision || pathEntry.getRevision() > endRevision ||
				!pathEntry.getPath().equals(path))
			{
				continue;
			}

			if (action == null || pathEntry.getAction().equals(action))
			{
				return true;
			}
		}

		return false;
	}


}
