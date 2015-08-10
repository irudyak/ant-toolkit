package com.anttoolkit.hadoop.tasks.yarn;

import java.io.*;
import java.text.*;
import java.util.*;
import java.util.regex.*;

import org.apache.tools.ant.*;
import org.apache.hadoop.yarn.api.records.*;
import org.apache.hadoop.yarn.exceptions.*;

public class ApplicationsListLoopTask
		extends GenericYarnTask
		implements TaskContainer
{
	private static final String APPLICATION_PATTERN = "%30s\t%20s\t%20s\t%10s\t%10s\t%18s\t%18s\t%15s\t%35s" + System.getProperty("line.separator");

	//filters
	private String appIdFilter;
	private String appNameFilter;
	private String appTypeFilter;
	private String userNameFilter;
	private String stateFilter;
	private String statusFilter;
	private String queueFilter;
	private String startTimeFromStr;
	private Date startTimeFrom;
	private String startTimeBeforeStr;
	private Date startTimeBefore;
	private String timeFormat = "MM/dd/yyyy hh:mm:ss aaa";
	private String locale = Locale.US.toString();
	private DateFormat dateFormat;

	private String appReportRef;

	private List<Task> tasks = new LinkedList<Task>();

	public void setAppReportRef(String reference)
	{
		appReportRef = reference;
	}

	public void setAppIdFilter(String filter)
	{
		appIdFilter = filter;
	}

	public void setAppNameFilter(String filter)
	{
		appNameFilter = filter;
	}

	public void setAppTypeFilter(String filter)
	{
		appTypeFilter = filter;
	}

	public void setUserNameFilter(String filter)
	{
		userNameFilter = filter;
	}

	public void setStateFilter(String filter)
	{
		stateFilter = filter == null ? null : filter.trim().toUpperCase();
	}

	public void setStatusFilter(String filter)
	{
		statusFilter = filter == null ? null : filter.trim().toUpperCase();
	}

	public void setQueueFilter(String filter)
	{
		queueFilter = filter;
	}

	public void setStartTimeFrom(String time)
	{
		startTimeFromStr = time;
	}

	public void setStartTimeBefore(String time)
	{
		startTimeBeforeStr = time;
	}

	public void setTimeFormat(String format)
	{
		timeFormat = format;
	}

	public void setLocale(String locale)
	{
		this.locale = locale;
	}

	@Override
	public void addTask(Task task)
	{
		tasks.add(task);
	}

	@Override
	protected void doHadoopWork() throws BuildException
	{
		List<ApplicationReport> apps;

		try
		{
			apps = getYarnClient().getApplications();
		}
		catch (YarnException e)
		{
			throw new BuildException("Failed to get list of YARN applications");
		}
		catch (IOException e)
		{
			throw new BuildException("Failed to get list of YARN applications");
		}

		if (appReportRef == null || appReportRef.trim().isEmpty())
		{
			log(String.format(APPLICATION_PATTERN, "Application-Id", "Application-Name",
			       "Application-Type", "User", "Queue", "State", "Final-State",
			       "Progress", "Tracking-URL"));
		}

		for (ApplicationReport report : apps)
		{
			if (!filter(report))
			{
				continue;
			}

			if (appReportRef == null || appReportRef.trim().isEmpty() || tasks.isEmpty())
			{
				printAppInfo(report);
				continue;
			}


			this.setReference(appReportRef, report);

			for (Task task : tasks)
			{
				task.perform();
			}
		}
	}

	@Override
	protected void hadoopValidate()
	{
		try
		{
			startTimeFrom = startTimeFromStr == null || startTimeFromStr.trim().isEmpty() ? null : dateFormat.parse(startTimeFromStr);
		}
		catch (ParseException e)
		{
			throw new BuildException("Incorrect start time from specified: " + startTimeFromStr);
		}

		try
		{
			startTimeBefore = startTimeBeforeStr == null || startTimeBeforeStr.trim().isEmpty() ? null : dateFormat.parse(startTimeBeforeStr);
		}
		catch (ParseException e)
		{
			throw new BuildException("Incorrect start time before specified: " + startTimeBeforeStr);
		}
	}

	private boolean filter(ApplicationReport report)
	{
		if (appIdFilter != null && !appIdFilter.trim().isEmpty() && !Pattern.matches(appIdFilter, report.getApplicationId().toString()))
		{
			return false;
		}

		if (appNameFilter != null && !appNameFilter.trim().isEmpty() && !Pattern.matches(appNameFilter, report.getName()))
		{
			return false;
		}

		if (appTypeFilter != null && !appTypeFilter.trim().isEmpty() && !Pattern.matches(appTypeFilter, report.getApplicationType()))
		{
			return false;
		}

		if (userNameFilter != null && !userNameFilter.trim().isEmpty() && !Pattern.matches(userNameFilter, report.getUser()))
		{
			return false;
		}

		if (stateFilter != null && !stateFilter.trim().isEmpty() && !Pattern.matches(stateFilter, report.getYarnApplicationState().toString()))
		{
			return false;
		}

		if (statusFilter != null && !statusFilter.trim().isEmpty() && !Pattern.matches(statusFilter, report.getFinalApplicationStatus().toString()))
		{
			return false;
		}

		if (queueFilter != null && !queueFilter.trim().isEmpty() && !Pattern.matches(queueFilter, report.getQueue()))
		{
			return false;
		}

		if (startTimeFrom != null && startTimeFrom.compareTo(new Date(report.getStartTime())) < 0)
		{
			return false;
		}

		if (startTimeBefore != null && startTimeBefore.compareTo(new Date(report.getStartTime())) > 0)
		{
			return false;
		}

		return true;
	}

	private void printAppInfo(ApplicationReport report)
	{
		DecimalFormat formatter = new DecimalFormat("###.##%");
		String progress = formatter.format(report.getProgress());

		log(String.format(APPLICATION_PATTERN, report.getApplicationId(),
			report.getName(), report.getApplicationType(), report.getUser(),
			report.getQueue(), report.getYarnApplicationState(),
			report.getFinalApplicationStatus(), progress, report.getOriginalTrackingUrl()));
	}
}
