package com.anttoolkit.hadoop.tasks.mapred;

import java.io.*;

import org.apache.hadoop.mapreduce.*;
import org.apache.tools.ant.*;

import com.anttoolkit.hadoop.tasks.hadoop.*;
import com.anttoolkit.general.tasks.collections.array.util.*;

public class GetJobEventsTask
		extends GenericHadoopTask
{
	private String jobId;
	private int fromEvent = 0;
	private int eventsCount = -1;
	private String statusArray;
	private String attemptIdsArray;
	private String logUrlsArray;

	public void setJobId(String jobId)
	{
		this.jobId = jobId;
	}

	public void setFromEvent(int from)
	{
		fromEvent = from;
	}

	public void setEventsCount(int count)
	{
		eventsCount = count;
	}

	public void setStatusArray(String array)
	{
		statusArray = array;
	}

	public void setAttemptIdsArray(String array)
	{
		attemptIdsArray = array;
	}

	public void setLogUrlsArray(String array)
	{
		logUrlsArray = array;
	}

	@Override
	protected void doHadoopWork() throws BuildException
	{
		Job job = getJob(jobId);
		if (job == null)
		{
			return;
		}

		TaskCompletionEvent[] events;

		try
		{
			events = eventsCount == -1 ?
					job.getTaskCompletionEvents(fromEvent) :
					job.getTaskCompletionEvents(fromEvent, eventsCount);
		}
		catch (IOException e)
		{
			throw new BuildException("Failed to take events for job " + jobId, e);
		}
		catch (InterruptedException e)
		{
			throw new BuildException("Failed to take events for job " + jobId, e);
		}

		if (events == null)
		{
			return;
		}

		for (TaskCompletionEvent event : events)
		{
			if (statusArray == null && attemptIdsArray == null && logUrlsArray == null)
			{
				log(event.getStatus() + " " + event.getTaskAttemptId() + " " + getTaskLogURL(event));
				continue;
			}

			if (statusArray != null)
			{
				ArrayManager.add(statusArray, event.getStatus().toString());
			}

			if (attemptIdsArray != null)
			{
				ArrayManager.add(attemptIdsArray, event.getTaskAttemptId().toString());
			}

			if (logUrlsArray != null)
			{
				ArrayManager.add(logUrlsArray, getTaskLogURL(event));
			}
		}
	}

	@Override
	protected void hadoopValidate()
	{
		if (jobId == null || jobId.trim().isEmpty())
		{
			throw new BuildException("Job id should be specified");
		}
	}

	private String getTaskLogURL(TaskCompletionEvent event)
	{
   		return (event.getTaskTrackerHttp() + "/tasklog?plaintext=true&attemptid=" + event.getTaskAttemptId());
 	}
}
