package com.anttoolkit.maestro.tasks.report;

import org.apache.tools.ant.*;

import com.maestro.sdk.client.command.*;
import com.maestro.sdk.client.command.report.*;
import com.maestro.sdk.client.exception.*;

import com.anttoolkit.maestro.tasks.*;

public class InstanceReportTask extends ApiCommandTask
{
	private Integer year;
	private Integer month;
	private Integer day;
	private String instance;
	private String property;

	public void setYear(int year)
	{
		this.year = year;
	}

	public void setMonth(int month)
	{
		this.month = month;
	}

	public void setDay(int day)
	{
		this.day = day;
	}

	public void setInstance(String instance)
	{
		this.instance = instance;
	}

	public void setProperty(String property)
	{
		this.property = property;
	}

	@Override
	public void doWork() throws BuildException
	{
		GetInstanceReportResult result;

		try
		{
			GetInstanceReport cmd = new GetInstanceReport(getMaestroRegion(), getMaestroProject(), instance, year, month, day);
			result = executeCommand(cmd);
		}
		catch (MaestroApiException e)
		{
			if (e instanceof MaestroExecutionException && ((MaestroExecutionException)e).getError().getCode() == -32007) {
				this.setPropertyThreadSafe(property, "0");
				return;
			}

			throw new BuildException("Failed to get cost report for instance " + instance, e);
		}

		this.setPropertyThreadSafe(property, result.getTotalPrice().toString());
	}

	@Override
	protected void validate()
	{
		if (year == null)
		{
			throw new BuildException("Year should be specified");
		}

		if (month == null)
		{
			throw new BuildException("Month should be specified");
		}

		if (instance == null)
		{
			throw new BuildException("Instance should be specified");
		}

		if (property == null)
		{
			throw new BuildException("Property to store instance price should be specified");
		}
	}
}
