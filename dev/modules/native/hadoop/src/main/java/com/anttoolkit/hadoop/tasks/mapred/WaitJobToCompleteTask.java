package com.anttoolkit.hadoop.tasks.mapred;

import org.apache.hadoop.mapreduce.*;
import org.apache.tools.ant.*;

import com.anttoolkit.hadoop.tasks.hadoop.*;

public class WaitJobToCompleteTask
		extends GenericHadoopTask
{
	private String jobId;
	private String stateProperty;
	private boolean failIfNotSucceed = true;
	private boolean failIfTimeOutExceed = true;
	private long sleepTimeout = 10000; // 10 seconds by default
	private long waitTime = 0;	// infinite by default

	public void setJobId(String jobId)
	{
		this.jobId = jobId;
	}

	public void setStateProperty(String property)
	{
		stateProperty = property;
	}

	public void setFailIfNotSucceed(boolean fail)
	{
		failIfNotSucceed = fail;
	}

	public void setFailIfTimeOutExceed(boolean fail)
	{
		failIfTimeOutExceed = fail;
	}

	public void setSleepTimeout(String sleepTimeout)
	{
		this.sleepTimeout = parseTimeout(sleepTimeout);
	}

	public void setWaitTime(String waitTime)
	{
		this.waitTime = parseTimeout(waitTime);
	}

	@Override
	protected void doHadoopWork() throws BuildException
	{
		long startTime = System.currentTimeMillis();
		long currentTime = startTime;

		while (waitTime == 0 || currentTime - startTime <= waitTime)
		{
			JobStatus.State state = getJobState(jobId);
			if (state == null || JobStatus.State.SUCCEEDED.equals(state))
			{
				if (stateProperty != null)
				{
					this.setPropertyThreadSafe(stateProperty, state == null ? "" : state.toString());
				}

				return;
			}

			if (JobStatus.State.FAILED.equals(state) ||
				JobStatus.State.KILLED.equals(state))
			{
				if (stateProperty != null)
				{
					this.setPropertyThreadSafe(stateProperty, state.toString());
				}

				if (!failIfNotSucceed)
				{
					return;
				}

				if (JobStatus.State.FAILED.equals(state))
				{
					throw new BuildException("Job " + jobId + " failed to complete successfully");
				}
				else
				{
					throw new BuildException("Job " + jobId + " was killed");
				}
			}

			//wait before starting next cycle
			try
			{
				Thread.sleep(sleepTimeout);
			}
			catch (InterruptedException e)
			{
				throw new BuildException("Wait cycle was interrupted", e);
			}

			currentTime = System.currentTimeMillis();
		}

		JobStatus.State state = getJobState(jobId);
		if (stateProperty != null)
		{
			this.setPropertyThreadSafe(stateProperty, state == null ? "" : state.toString());
		}

		if (state == null || JobStatus.State.SUCCEEDED.equals(state))
		{
			return;
		}

		if (JobStatus.State.FAILED.equals(state) ||
			JobStatus.State.KILLED.equals(state))
		{
			if (!failIfNotSucceed)
			{
				return;
			}

			if (JobStatus.State.FAILED.equals(state))
			{
				throw new BuildException("Job " + jobId + " failed to complete successfully");
			}
			else
			{
				throw new BuildException("Job " + jobId + " was killed");
			}
		}

		if (failIfTimeOutExceed)
		{
			throw new BuildException("Job " + jobId + " waiting timeout exceed but it is still not finished, current state is " + state.toString());
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

	private long parseTimeout(String timeout)
	{
		try
		{
			if (timeout.endsWith("s"))
			{
				return Long.parseLong(timeout.substring(0, timeout.length() - 1)) * 1000;
			}
			else if (timeout.endsWith("m"))
			{
				return Long.parseLong(timeout.substring(0, timeout.length() - 1)) * 60000;
			}
			else if (timeout.endsWith("h"))
			{
				return Long.parseLong(timeout.substring(0, timeout.length() - 1)) * 3600000;
			}
			else if (timeout.endsWith("d"))
			{
				return Long.parseLong(timeout.substring(0, timeout.length() - 1)) * 86400000;
			}

			return Long.parseLong(timeout.substring(0, timeout.length()));
		}
		catch (NumberFormatException e)
		{
			throw new BuildException("Invalid timeout specification: " + timeout, e);
		}
	}
}
