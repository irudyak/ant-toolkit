package com.anttoolkit.hadoop.tasks.yarn;

import org.apache.hadoop.yarn.api.records.*;
import org.apache.tools.ant.*;

public class WaitAppToCompleteTask extends GenericYarnTask
{
	private String appId;
	private String stateProperty;
	private boolean failIfNotSucceed = true;
	private boolean failIfTimeOutExceed = true;
	private long sleepTimeout = 10000; // 10 seconds by default
	private long waitTime = 0;	// infinite by default

	public void setAppId(String appId)
	{
		this.appId = appId;
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
			YarnApplicationState state = getApplicationState(appId);
			if (state == null || YarnApplicationState.FINISHED.equals(state))
			{
				if (stateProperty != null)
				{
					this.setPropertyThreadSafe(stateProperty, state == null ? "" : state.toString());
				}

				return;
			}

			if (YarnApplicationState.FAILED.equals(state) ||
				YarnApplicationState.KILLED.equals(state))
			{
				if (stateProperty != null)
				{
					this.setPropertyThreadSafe(stateProperty, state.toString());
				}

				if (!failIfNotSucceed)
				{
					return;
				}

				if (YarnApplicationState.FAILED.equals(state))
				{
					throw new BuildException("Application " + appId + " failed to complete successfully");
				}
				else
				{
					throw new BuildException("Application " + appId + " was killed");
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

		YarnApplicationState state = getApplicationState(appId);
		if (stateProperty != null)
		{
			this.setPropertyThreadSafe(stateProperty, state == null ? "" : state.toString());
		}

		if (state == null || YarnApplicationState.FINISHED.equals(state))
		{
			return;
		}

		if (YarnApplicationState.FAILED.equals(state) ||
			YarnApplicationState.KILLED.equals(state))
		{
			if (!failIfNotSucceed)
			{
				return;
			}

			if (YarnApplicationState.FAILED.equals(state))
			{
				throw new BuildException("Application " + appId + " failed to complete successfully");
			}
			else
			{
				throw new BuildException("Application " + appId + " was killed");
			}
		}

		if (failIfTimeOutExceed)
		{
			throw new BuildException("Application " + appId + " waiting timeout exceed but it is still not finished, current state is " + state.toString());
		}
	}

	@Override
	protected void hadoopValidate()
	{
		if (appId == null || appId.trim().isEmpty())
		{
			throw new BuildException("Application id should be specified");
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
