package com.anttoolkit.maestro.tasks;

import com.maestro.cli.service.*;
import com.maestro.sdk.client.command.*;
import com.maestro.sdk.client.exception.*;

import com.anttoolkit.maestro.common.*;

public abstract class ApiCommandTask
		extends GenericMaestroTask
{
	private String project;
	private String region;

	public void setProject(String project)
	{
		this.project = project;
	}

	public void setRegion(String region)
	{
		this.region = region;
	}

	protected String getMaestroProject()
	{
		return this.project;
	}

	protected String getMaestroRegion()
	{
		return this.region;
	}

	protected <T> T executeCommand(Command<T> command) throws MaestroApiException
	{
		getExecutionService().setCredentials(MaestroSessionManager.getUserId(this), MaestroSessionManager.getToken(this));
		return getExecutionService().execute(command);
	}

	protected IApiExecutionService getExecutionService()
	{
		return MaestroHelper.getApiExecutionService();
	}
}
