package com.anttoolkit.hadoop.tasks.mapred;

import java.io.*;

import org.apache.hadoop.mapreduce.*;
import org.apache.tools.ant.*;

import com.anttoolkit.hadoop.tasks.hadoop.*;

public class KillJobTask extends GenericHadoopTask
{
	private String jobId;

	public void setJobId(String jobId)
	{
		this.jobId = jobId;
	}

	@Override
	protected void doHadoopWork() throws BuildException
	{
		try
		{
			Job job = getJob(jobId);
			if (job == null)
			{
				log("There is no job " + jobId + " among running jobs");
			}
			else
			{
				job.killJob();
				log("MapReduce job " + jobId + " was killed");
			}
		}
		catch (IOException e)
		{
			throw new BuildException("Failed to kill job: " + jobId);
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
}
