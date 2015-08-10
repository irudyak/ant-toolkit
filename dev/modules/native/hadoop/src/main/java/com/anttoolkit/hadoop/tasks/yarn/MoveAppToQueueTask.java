package com.anttoolkit.hadoop.tasks.yarn;

import org.apache.hadoop.yarn.api.records.*;
import org.apache.hadoop.yarn.exceptions.*;
import org.apache.hadoop.yarn.util.*;
import org.apache.tools.ant.*;

public class MoveAppToQueueTask extends GenericYarnTask
{
	private String applicationId;
	private String queue;

	public void setApplicationId(String id)
	{
		this.applicationId = id;
	}

	public void setQueue(String queue)
	{
		this.queue = queue;
	}

	@Override
	protected void doHadoopWork() throws BuildException
	{
		ApplicationReport appReport = getApplicationReport(applicationId);
		if (appReport == null ||
			appReport.getYarnApplicationState() == YarnApplicationState.FINISHED ||
			appReport.getYarnApplicationState() == YarnApplicationState.KILLED ||
			appReport.getYarnApplicationState() == YarnApplicationState.FAILED)
		{
			return;
		}

		try
		{
			getYarnClient().moveApplicationAcrossQueues(ConverterUtils.toApplicationId(applicationId), queue);
		}
		catch (ApplicationNotFoundException e)
		{
		}
		catch (Throwable e)
		{
			throw new BuildException("Failed to move application " + applicationId + " to queue " + queue, e);
		}
	}

	@Override
	protected void hadoopValidate()
	{
		if (applicationId == null || applicationId.trim().isEmpty())
		{
			throw new BuildException("ApplicationId should be specified");
		}

		if (queue == null || queue.trim().isEmpty())
		{
			throw new BuildException("Queue should be specified");
		}
	}

}

