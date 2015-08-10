package com.anttoolkit.documentum.tasks.workflow;

import com.documentum.fc.client.IDfWorkflow;
import org.apache.tools.ant.*;

import com.documentum.fc.common.*;

import com.anttoolkit.documentum.common.*;

import java.text.MessageFormat;

public class WaitForWorkflowCompletionTask
	extends GenericDocbaseTask
{
	private static final String DQL_GET_WORKFLOW_STATE = "select r_runtime_state " +
			"from dm_workflow where r_object_id=''{0}''";

	private String workflowId;
	private String sleepTimeout;
	private String waitTime;

	public void setWorkflowId(String workflowId)
	{
		this.workflowId = workflowId;
	}

	public void setSleepTimeout(String sleepTimeout)
	{
		this.sleepTimeout = sleepTimeout;
	}

	public void setWaitTime(String waitTime)
	{
		this.waitTime = waitTime;
	}

	@Override
	public void doWork() throws BuildException
	{
		long startTime = System.currentTimeMillis();
		long currentTime = startTime;
		long waitTime = getWaitTime();
		long sleepTimeout = getSleepTimeout();

		while (currentTime - startTime < waitTime)
		{
			try
			{
				String dqlQuery = MessageFormat.format(DQL_GET_WORKFLOW_STATE, workflowId);
				int state = DqlHelper.getIntegerParamFromFirstString(getSession(), dqlQuery);
				if (state == IDfWorkflow.DF_WF_STATE_FINISHED)
				{
					return;
				}
			}
			catch (DfEndOfCollectionException e)
			{
				return;
			}
			catch (DfException e)
			{
				throw new BuildException("Failed to check the state of workflow " + workflowId, e);
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

		throw new BuildException("Wait time exceed " + waitTime + "ms, " +
				"but workflow " + workflowId + " still not completed");
	}

	protected void validate()
	{
		if (workflowId == null)
		{
			throw new BuildException("Workflow id should be specified");
		}
	}

	private long getSleepTimeout()
	{
		if (sleepTimeout == null)
		{
			return 1000;	// 1 second by default
		}

		return parseTimeout(sleepTimeout);
	}

	private long getWaitTime()
	{
		if (waitTime == null)
		{
			return 300000;	// 5 minutes by default
		}

		return parseTimeout(waitTime);
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
