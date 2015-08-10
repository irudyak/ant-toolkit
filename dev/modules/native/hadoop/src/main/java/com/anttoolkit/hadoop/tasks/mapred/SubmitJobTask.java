package com.anttoolkit.hadoop.tasks.mapred;

import org.apache.tools.ant.*;
import org.apache.hadoop.mapreduce.*;

import com.anttoolkit.hadoop.tasks.hadoop.*;

public class SubmitJobTask extends GenericHadoopTask
{
	private String jobIdProperty;
	private String ref;
	private boolean async = true;

	public void setJobIdProperty(String prop)
	{
		jobIdProperty = prop;
	}

	public void setRefid(String reference)
	{
		this.ref = reference;
	}

	public void setAsync(boolean async)
	{
		this.async = async;
	}

	@Override
	protected void doHadoopWork() throws BuildException
	{
		try
		{
			Job job = (Job)getReference(ref);

			if (!async)
			{
				job.waitForCompletion(true);
				return;
			}
			
			job.submit();
			log("New MapReduce job submitted: " + job.getJobID().toString());

			if (jobIdProperty != null)
			{
				this.setPropertyThreadSafe(jobIdProperty, job.getJobID().toString());
			}
		}
		catch (Throwable e)
		{
			throw new BuildException("Exception occured while trying to submit new MapReduce job specified by reference '" + ref + "'", e);
		}
	}

	@Override
	protected void hadoopValidate()
	{
		if (ref == null || ref.trim().isEmpty())
		{
			throw new BuildException("Hadoop job reference should be specified");
		}

		Object obj = getReference(ref);
		if (obj == null)
		{
			throw new BuildException("Specified Hadoop job reference '" + ref + "' doesn't exist");
		}

		if (!(obj instanceof Job))
		{
			throw new BuildException("Specified Hadoop job reference '" + ref + "' doesn't contain valid Job object");
		}
	}
}
