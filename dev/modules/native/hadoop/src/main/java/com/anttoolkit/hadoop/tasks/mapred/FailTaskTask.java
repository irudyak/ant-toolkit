package com.anttoolkit.hadoop.tasks.mapred;

import java.io.*;

import org.apache.hadoop.mapreduce.*;
import org.apache.tools.ant.*;

import com.anttoolkit.hadoop.tasks.hadoop.*;

public class FailTaskTask
		extends GenericHadoopTask
{
	private TaskAttemptID taskId;

	public void setTaskId(String id)
	{
		taskId = TaskAttemptID.forName(id);
	}

	@Override
	protected void doHadoopWork() throws BuildException
	{
		Job job = getJob(taskId.getJobID().toString());
		if (job == null)
		{
			log("There are no running job for the task " + taskId.toString());
			return;
		}

		try
		{
			if (!job.killTask(taskId, true))
			{
				log("Failed to fail task " + taskId.toString());
			}
		}
		catch (IOException e)
		{
			throw new BuildException("Failed to fail task " + taskId.toString(), e);
		}
	}

	@Override
	protected void hadoopValidate()
	{
		if (taskId == null)
		{
			throw new BuildException("Task id should be specified");
		}
	}
}
