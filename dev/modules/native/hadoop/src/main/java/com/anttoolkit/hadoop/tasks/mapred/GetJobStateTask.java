package com.anttoolkit.hadoop.tasks.mapred;

import java.io.*;

import org.apache.hadoop.mapreduce.*;
import org.apache.tools.ant.*;

import com.anttoolkit.hadoop.tasks.hadoop.*;

public class GetJobStateTask
		extends GenericHadoopTask
{
	private String jobId;
	private String property;

	public void setJobId(String jobId)
	{
		this.jobId = jobId;
	}

	public void setProperty(String property)
	{
		this.property = property;
	}

	@Override
	protected void doHadoopWork() throws BuildException
	{
		JobStatus.State state = getJobState(jobId);
		if (state == null)
		{
			throw new BuildException("Job " + jobId + " doesn't exist in a cluster or may be you don't have enough rights to access it");
		}

		if (property != null && !property.trim().isEmpty())
		{
			this.setPropertyThreadSafe(property, state.toString());
		}
		else
		{
			log("Job " + jobId + " state is " + state.toString());
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
