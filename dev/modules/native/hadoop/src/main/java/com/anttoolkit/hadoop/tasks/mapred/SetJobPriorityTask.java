package com.anttoolkit.hadoop.tasks.mapred;

import org.apache.hadoop.mapreduce.*;
import org.apache.tools.ant.*;

import com.anttoolkit.hadoop.tasks.hadoop.*;

import java.io.*;

public class SetJobPriorityTask
		extends GenericHadoopTask
{
	private String jobId;
	private JobPriority priority;

	public void setJobId(String id)
	{
		jobId = id;
	}

	public void setPriority(String priority)
	{
		this.priority = JobPriority.valueOf(priority.trim().toUpperCase());
	}

	@Override
	protected void doHadoopWork() throws BuildException
	{
		Job job = getJob(jobId);
		if (job == null)
		{
			JobStatus.State state = getJobState(jobId);
			if (state == null)
			{
				log("There is no " + jobId + " job in Hadoop cluster, so no priority will be changed");
			}
			else
			{
				log("Job " + jobId + " already completed, so no priority will be changed");
			}

			return;
		}

		try
		{
			job.setPriority(priority);
		}
		catch (IOException e)
		{
			throw new BuildException("Failed to change priority of the job " + jobId, e);
		}
		catch (InterruptedException e)
		{
			throw new BuildException("Failed to change priority of the job " + jobId, e);
		}
	}

	@Override
	protected void hadoopValidate()
	{
		if (jobId == null || jobId.trim().isEmpty())
		{
			throw new BuildException("Job id should be specified");
		}

		if (priority == null)
		{
			throw new BuildException("Job priority should be specified");
		}
	}
}
