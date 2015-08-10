package com.anttoolkit.hadoop.tasks.yarn;

import org.apache.hadoop.conf.*;
import org.apache.hadoop.yarn.api.records.*;
import org.apache.hadoop.yarn.client.api.*;
import org.apache.hadoop.yarn.conf.*;
import org.apache.hadoop.yarn.exceptions.*;
import org.apache.hadoop.yarn.util.*;
import org.apache.tools.ant.*;

import com.anttoolkit.hadoop.tasks.hadoop.*;

import java.io.IOException;

public abstract class GenericYarnTask extends GenericHadoopTask
{
	private YarnClient client;

	@Override
	protected Configuration getConfiguration()
	{
		return new YarnConfiguration(super.getConfiguration());
	}

	protected YarnClient getYarnClient()
	{
		if (client != null)
		{
			return client;
		}

		synchronized (this)
		{
			if (client != null)
			{
				return client;
			}

			client = YarnClient.createYarnClient();
			client.init(getConfiguration());
			client.start();

			return client;
		}
	}

	protected ApplicationId getApplicationId(String applicationId)
	{
		try
		{
			return ConverterUtils.toApplicationId(applicationId);
		}
		catch (Exception e)
		{
			throw new BuildException("Invalid ApplicationId specified");
		}
	}

	protected ApplicationReport getApplicationReport(String applicationId)
	{
		try
		{
			return this.getYarnClient().getApplicationReport(ConverterUtils.toApplicationId(applicationId));
		}
		catch (ApplicationNotFoundException e)
		{
			return null;
		}
		catch (Throwable e)
		{
			throw new BuildException("Failed to get report for the YARN application: " + applicationId, e);
		}
	}

	protected YarnApplicationState getApplicationState(String applicationId)
	{
		try
		{
			ApplicationReport appReport = getYarnClient().getApplicationReport(getApplicationId(applicationId));
			return appReport.getYarnApplicationState();
		}
		catch(Throwable e)
		{
			throw new BuildException("Failed to get report for YARn application: " + applicationId, e);
		}
	}

	protected ContainerReport getContainerReport(String containerId)
	{
		try
		{
			return getYarnClient().getContainerReport(ConverterUtils.toContainerId(containerId));
		}
		catch (Throwable e)
		{
			throw new BuildException("Failed to get report for YARN container: " + containerId, e);
		}
	}

	protected boolean isApplicationFinished(YarnApplicationState appState)
	{
		return appState == YarnApplicationState.FINISHED ||
				appState == YarnApplicationState.FAILED ||
				appState == YarnApplicationState.KILLED;
	}


	protected void releaseHadoopResources()
	{
		synchronized (this)
		{
			if (client == null)
			{
				return;
			}

			try
			{
				client.stop();
				client.close();
			}
			catch (Throwable e)
			{
			}
			finally
			{
				client = null;
			}
		}

		super.releaseHadoopResources();
	}
}
