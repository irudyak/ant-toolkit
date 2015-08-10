package com.anttoolkit.hadoop.tasks.yarn;

import org.apache.hadoop.yarn.api.records.*;
import org.apache.hadoop.yarn.exceptions.*;
import org.apache.hadoop.yarn.util.*;
import org.apache.tools.ant.*;

public class KillAppTask extends GenericYarnTask
{
	private String applicationId;

	public void setApplicationId(String id)
	{
		this.applicationId = id;
	}

	@Override
	protected void doHadoopWork() throws BuildException
	{
		ApplicationReport appReport = getApplicationReport(applicationId);

		if (appReport == null)
		{
			return;
		}

		if (appReport.getYarnApplicationState() == YarnApplicationState.FINISHED
			|| appReport.getYarnApplicationState() == YarnApplicationState.KILLED
			|| appReport.getYarnApplicationState() == YarnApplicationState.FAILED)
		{
			return;
		}

		try
		{
			this.getYarnClient().killApplication(ConverterUtils.toApplicationId(applicationId));
		}
		catch (ApplicationNotFoundException e)
		{
		}
		catch (Throwable e)
		{
			throw new BuildException("Failed to kill YARN application: " + applicationId, e);
		}
	}

	@Override
	protected void hadoopValidate()
	{
		if (applicationId == null || applicationId.trim().isEmpty())
		{
			throw new BuildException("ApplicationId should be specified");
		}
	}
}
