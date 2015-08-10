package com.anttoolkit.hadoop.tasks.mapred;

import java.io.*;
import java.text.*;
import java.util.*;
import java.util.regex.*;

import org.apache.hadoop.mapreduce.*;
import org.apache.tools.ant.*;

import com.anttoolkit.general.refs.*;
import com.anttoolkit.hadoop.tasks.hadoop.*;

public class JobsListLoopTask
		extends GenericHadoopTask
		implements TaskContainer
{
	private static final String HEADER_PATTERN = "%23s\t%10s\t%14s\t%12s\t%12s\t%10s\t%15s\t%15s\t%8s\t%8s\t%10s\t%10s\n";
	private static final String DATA_PATTERN = "%23s\t%10s\t%14d\t%12s\t%12s\t%10s\t%15s\t%15s\t%8s\t%8s\t%10s\t%10s\n";
	private static final String MEM_PATTERN = "%dM";
	private static final String UNAVAILABLE = "N/A";

	//filters
	private String jobIdFilter;
	private String jobNameFilter;
	private String userNameFilter;
	private String stateFilter;
	private String priorityFilter;
	private String queueFilter;
	private String startTimeFromStr;
	private Date startTimeFrom;
	private String startTimeBeforeStr;
	private Date startTimeBefore;
	private String timeFormat = "MM/dd/yyyy hh:mm:ss aaa";
	private String locale = Locale.US.toString();
	private DateFormat dateFormat;

	//properties
	private String jobStatusRef;

	//tasks
	private List<Task> tasks = new LinkedList<Task>();

	public void setJobStatusRef(String ref)
	{
		jobStatusRef = ref;
	}

	public void setJobIdFilter(String filter)
	{
		jobIdFilter = filter;
	}

	public void setJobNameFilter(String filter)
	{
		jobNameFilter = filter;
	}

	public void setUserNameFilter(String filter)
	{
		userNameFilter = filter;
	}

	public void setStateFilter(String filter)
	{
		stateFilter = filter.toUpperCase().trim();
	}

	public void setPriorityFilter(String filter)
	{
		priorityFilter = filter.toUpperCase().trim();
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
	protected void doHadoopWork() throws BuildException
	{
		try
		{
			dateFormat = new SimpleDateFormat(timeFormat, new Locale(locale));
		}
		catch (Throwable e)
		{
			throw new BuildException("Incorrect time format/locale specified: " + timeFormat + "/" + locale, e);
		}

		JobStatus[] statuses;

		try
		{
			statuses = getCluster().getAllJobStatuses();
		}
		catch (IOException e)
		{
			throw new BuildException("Failed to get cluster jobs", e);
		}
		catch (InterruptedException e)
		{
			throw new BuildException("Failed to get cluster jobs", e);
		}

		if (jobStatusRef == null || jobStatusRef.trim().isEmpty())
		{
			log(String.format(HEADER_PATTERN, "JobId", "State", "StartTime", "UserName",
					"Queue", "Priority", "UsedContainers", "RsvdContainers",
					"UsedMem", "RsvdMem", "NeededMem", "AM info"));
		}

		for (JobStatus status : statuses)
		{
			if (!filter(status))
			{
				continue;
			}

			if (jobStatusRef == null || jobStatusRef.trim().isEmpty() || tasks.isEmpty())
			{
				printJobInfo(status);
				continue;
			}

			this.setReference(jobStatusRef, status);

			for (Task task : tasks)
			{
				task.perform();
			}
		}
	}

	@Override
	public void addTask(Task task)
	{
		tasks.add(task);
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

	private boolean filter(JobStatus status)
	{
		if (jobIdFilter != null && !jobIdFilter.trim().isEmpty() && !Pattern.matches(jobIdFilter, status.getJobID().toString()))
		{
			return false;
		}

		if (jobNameFilter != null && !jobNameFilter.trim().isEmpty() && !Pattern.matches(jobNameFilter, status.getJobName()))
		{
			return false;
		}

		if (userNameFilter != null && !userNameFilter.trim().isEmpty() && !Pattern.matches(userNameFilter, status.getUsername()))
		{
			return false;
		}

		if (stateFilter != null && !stateFilter.trim().isEmpty() && !Pattern.matches(stateFilter, status.getState().toString()))
		{
			return false;
		}

		if (priorityFilter != null && !priorityFilter.trim().isEmpty() && !Pattern.matches(priorityFilter, status.getPriority().toString()))
		{
			return false;
		}

		if (queueFilter != null && !queueFilter.trim().isEmpty() && !Pattern.matches(queueFilter, status.getQueue()))
		{
			return false;
		}

		if (queueFilter != null && !queueFilter.trim().isEmpty() && !Pattern.matches(queueFilter, status.getQueue()))
		{
			return false;
		}

		if (startTimeFrom != null && startTimeFrom.compareTo(new Date(status.getStartTime())) < 0)
		{
			return false;
		}

		if (startTimeBefore != null && startTimeBefore.compareTo(new Date(status.getStartTime())) > 0)
		{
			return false;
		}

		return true;
	}

	private void printJobInfo(JobStatus status)
	{
		int numUsedSlots = status.getNumUsedSlots();
		int numReservedSlots = status.getNumReservedSlots();
		int usedMem = status.getUsedMem();
		int rsvdMem = status.getReservedMem();
		int neededMem = status.getNeededMem();

		String info = String.format(DATA_PATTERN,
				status.getJobID().toString(), status.getState(), status.getStartTime(),
				status.getUsername(), status.getQueue(),
				status.getPriority().name(),
				numUsedSlots < 0 ? UNAVAILABLE : numUsedSlots,
				numReservedSlots < 0 ? UNAVAILABLE : numReservedSlots,
				usedMem < 0 ? UNAVAILABLE : String.format(MEM_PATTERN, usedMem),
				rsvdMem < 0 ? UNAVAILABLE : String.format(MEM_PATTERN, rsvdMem),
				neededMem < 0 ? UNAVAILABLE : String.format(MEM_PATTERN, neededMem),
				status.getSchedulingInfo());

		log(info);
	}
}
