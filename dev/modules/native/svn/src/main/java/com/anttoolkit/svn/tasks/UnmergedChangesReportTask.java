package com.anttoolkit.svn.tasks;

import java.util.*;

import com.anttoolkit.general.tasks.collections.array.util.ArrayManager;
import com.anttoolkit.general.tasks.GenericTask;
import org.apache.tools.ant.*;
import org.apache.tools.ant.taskdefs.*;

import com.anttoolkit.svn.tasks.util.*;

public class UnmergedChangesReportTask
		extends GenericTask
{
	private String trunkFile = null;
	private String branchFile = null;
	private String trunkRoot = null;
	private String branchRoot = null;
	private String summaryReportFile = null;
	private String userReportsDir = null;
	private String usersArray = null;
	private String usersCountProperty = null;
	private String summaryReportXslt = null;
	private String userReportXslt = null;

	public void setTrunkLogFile(String file)
	{
		trunkFile = file;
	}

	public void setBranchLogFile(String file)
	{
		branchFile = file;
	}

	public void setTrunkRoot(String root)
	{
		trunkRoot = root;
	}

	public void setBranchRoot(String root)
	{
		branchRoot = root;
	}

	public void setSummaryReport(String file)
	{
		summaryReportFile = file;
	}

	public void setUserReportsDir(String dir)
	{
		userReportsDir = dir;
	}

	public void setUsersArray(String array)
	{
		usersArray = array;
	}

	public void setSummaryReportXsltStyle(String file)
	{
		summaryReportXslt = file;
	}

	public void setUserReportXsltStyle(String file)
	{
		userReportXslt = file;
	}

	public void setUsersCountProperty(String property)
	{
		usersCountProperty = property;
	}

	public void doWork() throws BuildException
	{
		initialize();

		LogParser parser = new LogParser();

		LogReport branchReport = parser.parseLog(getFileFullPath(branchFile));
		branchReport.removeEntriesNotStartedWithPath(branchRoot);
		branchReport.removeEntriesWithPath(branchRoot);
		branchReport.removeEntriesStartedWithPath(trunkRoot);
		MergeHelper.removeMutuallyExclusivePathEntriesFromBranchReport(branchReport);
		branchReport.removeNotLastModifications();

		if (branchReport.isEmpty())
		{
			this.log("No changes in branch log");
			return;
		}

		LogReport trunkReport = parser.parseLog(getFileFullPath(trunkFile));
		trunkReport.removeLogEntriesWithRevisionLowerThan(branchReport.getMinLogEntryRevision());

		List<String> authors = branchReport.getAuthors();

		LogReport summaryReport = new LogReport();

		int usersCount = 0;

		for (String author : authors)
		{
			LogReport authorReport = MergeHelper.getUnmergedChangesReport(trunkReport, branchReport, trunkRoot, branchRoot, author);
			if (authorReport == null || authorReport.isEmpty())
			{
				continue;
			}

			String authorReportFile = userReportsDir != null ? userReportsDir.trim().replace("\\", "/") : "";
			authorReportFile = authorReportFile.endsWith("/") ? authorReportFile : authorReportFile + "/";
			authorReportFile = authorReportFile + author + ".html";
			authorReportFile = getFileFullPath(authorReportFile);

			XSLTProcess xsltTask = new XSLTProcess();
			this.initDelegateTask(xsltTask);
			xsltTask.setStyle(userReportXslt);

			authorReport.writeReport(authorReportFile, xsltTask);

			summaryReport.union(authorReport);

			if (usersArray != null)
			{
				ArrayManager.add(usersArray, author);
			}

			usersCount++;
		}

		if (usersCountProperty != null)
		{
			this.setPropertyThreadSafe(usersCountProperty, Integer.toString(usersCount));
		}

		if (summaryReport.isEmpty())
		{
			return;
		}

		XSLTProcess xsltTask = new XSLTProcess();
		this.initDelegateTask(xsltTask);
		xsltTask.setStyle(summaryReportXslt);

		summaryReport.writeReport(getFileFullPath(summaryReportFile), xsltTask);
	}

	protected void validate()
	{
		if (trunkFile == null)
		{
			throw new BuildException("Trunk log file should be specified");
		}

		if (branchFile == null)
		{
			throw new BuildException("Branch log file should be specified");
		}

		if (trunkRoot == null)
		{
			throw new BuildException("Trunk root path is not specified");
		}

		if (branchRoot == null)
		{
			throw new BuildException("Branch root path is not specified");
		}

		if (summaryReportFile == null)
		{
			throw new BuildException("Summary report file should be specified");
		}

		if (summaryReportXslt == null)
		{
			throw new BuildException("Summary report XSLT file should be specified");
		}

		if (userReportXslt == null)
		{
			throw new BuildException("User report XSLT file should be specified");
		}
	}

	private void initialize()
	{
		if (usersArray != null)
		{
			ArrayManager.init(usersArray, new LinkedList(), false);
		}

		if (usersCountProperty != null)
		{
			this.setPropertyThreadSafe(usersCountProperty, "0");
		}
	}
}
