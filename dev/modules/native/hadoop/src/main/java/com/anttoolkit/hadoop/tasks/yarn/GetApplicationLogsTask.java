package com.anttoolkit.hadoop.tasks.yarn;

import java.io.*;
import java.util.*;
import javax.ws.rs.core.*;

import com.sun.jersey.api.client.*;

import org.apache.hadoop.yarn.api.records.*;
import org.apache.hadoop.yarn.conf.*;
import org.apache.hadoop.yarn.webapp.util.*;
import org.apache.tools.ant.*;

import org.codehaus.jettison.json.*;

import com.anttoolkit.hadoop.tasks.yarn.util.*;
import com.anttoolkit.general.common.*;

public class GetApplicationLogsTask extends GenericYarnTask
{
	private String applicationId;
	private String containerId;
	private String nodeAddress;
	private String appOwner;
	private String amLogs;

	private String logsFile;
	private String logsDir;

	public void setApplicationId(String id)
	{
		this.applicationId = id;
	}

	public void setContainerId(String id)
	{
		this.containerId = id;
	}

	public void setNodeAddress(String address)
	{
		this.nodeAddress = address;
	}

	public void setAppOwner(String owner)
	{
		this.appOwner = owner == null || owner.trim().isEmpty() ? null : owner.trim();
	}

	public void setAmLogs(String amLogs)
	{
		this.amLogs = amLogs;
	}

	public void setFile(String file)
	{
		this.logsFile = file;
	}

	public void setDir(String dir)
	{
		this.logsDir = dir;
	}

	@Override
	protected void doHadoopWork() throws BuildException
	{
		if (appOwner == null)
		{
			appOwner = getCurrentHadoopUser();
		}

		YarnApplicationState appState = getApplicationState(applicationId);

		if (appState == YarnApplicationState.NEW ||
				appState == YarnApplicationState.NEW_SAVING ||
				appState == YarnApplicationState.SUBMITTED)
		{
			throw new BuildException("Logs are not avaiable right now.");
		}

		dumpAmContainerLogs(appState);

		if (containerId == null)
		{
			LogsHelper helper = new LogsHelper(this.getConfiguration(), logsFile, logsDir);

			try
			{
				helper.dumpAllContainersLogs(getApplicationId(applicationId), appOwner);
			}
			catch (IOException e)
			{
				throw new BuildException("Failed to dump YARN logs for application: " + applicationId, e);
			}

			return;
		}

		// if we provide the node address and the application is in the final
		// state, we could directly get logs from HDFS.
		if (nodeAddress != null && isApplicationFinished(appState))
		{
			printContainerLogs(containerId, nodeAddress);
			return;
		}

		// If the nodeAddress is not provided, we will try to get
		// the ContainerReport. In the containerReport, we could get
		// nodeAddress and nodeHttpAddress
		try
		{
			ContainerReport report = getContainerReport(containerId);
			String nodeId = report.getAssignedNode().toString();

			// If the application is not in the final state,
			// we will provide the NodeHttpAddress and get the container logs
			// by calling NodeManager webservice.
			if (!isApplicationFinished(appState))
			{
				printContainerLogs(containerId, nodeAddress != null ? nodeAddress : nodeId);
			}
			else
			{
				// If the application is in the final state, we will directly
				// get the container logs from HDFS.
				printContainerLogs(containerId, nodeId);
			}
		}
		catch (Throwable e)
		{
			throw new BuildException("Unable to get logs for this container '"
					+ containerId + "' for the application '" + applicationId + "'", e);
		}
	}

	@Override
	protected void hadoopValidate()
	{
		if (applicationId == null || applicationId.trim().isEmpty())
		{
			throw new BuildException("Application ID should be specified");
		}

		if (containerId == null && nodeAddress != null)
		{
			throw new BuildException("Container ID should be specified");
		}

		if (logsFile == null && logsDir == null)
		{
			throw new BuildException("Logs file or logs dir should be specified");
		}

		if (logsFile != null && logsDir != null)
		{
			throw new BuildException("Either logs file or logs dir should be specified");
		}

		if (logsFile != null && this.dirExists(logsFile))
		{
			throw new BuildException("Can't write logs to file \"" + logsFile + "\" cause directory with the same name already exists");
		}

		if (logsDir != null && this.fileExists(logsDir))
		{
			throw new BuildException("Can't write logs to dir \"" + logsDir + "\" cause file with the same name already exists");
		}

		if (logsDir != null && !this.dirExists(logsDir))
		{
			throw new BuildException("Specified logs directory \"" + logsDir + "\" doesn't exist");
		}
	}

	protected List<String> getAmContainers()
	{
		if (amLogs == null || amLogs.trim().isEmpty())
		{
			return null;
		}

		String[] logs = amLogs.split(",");

		List<String> containers = new LinkedList<String>();

		for (String log : logs)
		{
			if (!log.trim().isEmpty())
			{
				containers.add(log.trim());
			}
		}

		return containers.isEmpty() ? null : containers;
	}

	protected void dumpAmContainerLogs(YarnApplicationState appState)
	{
		List<String> amContainers = getAmContainers();
		if (amContainers == null || amContainers.isEmpty())
		{
			return;
		}

		// If the application is running, we will call the RM WebService
		// to get the AppAttempts which includes the nodeHttpAddress
		// and containerId for all the AM Containers.
		// After that, we will call NodeManager webService to get the
		// related logs
		if (appState == YarnApplicationState.ACCEPTED || appState == YarnApplicationState.RUNNING)
		{
			printAMContainerLogs(amContainers, false);
			return;
		}

		// If the application is in the final state, we will call RM webservice
		// to get all AppAttempts information first. If we get nothing,
		// we will try to call AHS webservice to get related AppAttempts
		// which includes nodeAddress for the AM Containers.
		// After that, we will use nodeAddress and containerId
		// to get logs from HDFS directly.
		if (this.getConfiguration().getBoolean(
				YarnConfiguration.APPLICATION_HISTORY_ENABLED,
				YarnConfiguration.DEFAULT_APPLICATION_HISTORY_ENABLED))
		{
			printAMContainerLogs(amContainers, true);
			return;
		}

		throw new BuildException("Can not get AMContainers logs for the application:" + applicationId
				+ ". This application is finished."
				+ " Please enable the application history service. Or Using "
				+ "yarn logs -applicationId <appId> -containerId <containerId> "
				+ "--nodeAddress <nodeHttpAddress> to get the container logs");
	}

	private List<JSONObject> getAMContainerInfoForRMWebService()
			throws ClientHandlerException, UniformInterfaceException, JSONException
	{
		Client webServiceClient = Client.create();
		String webAppAddress = WebAppUtils.getWebAppBindURL(getConfiguration(), YarnConfiguration.RM_BIND_HOST,
				WebAppUtils.getRMWebAppURLWithScheme(getConfiguration()));

		WebResource webResource = webServiceClient.resource(webAppAddress);

		ClientResponse response = webResource.path("ws").path("v1").path("cluster").path("apps")
				.path(applicationId).path("appattempts").accept(MediaType.APPLICATION_JSON)
				.get(ClientResponse.class);

		JSONObject json = response.getEntity(JSONObject.class).getJSONObject("appAttempts");
		JSONArray requests = json.getJSONArray("appAttempt");

		List<JSONObject> amContainersList = new ArrayList<JSONObject>();
		for (int i = 0; i < requests.length(); i++)
		{
			amContainersList.add(requests.getJSONObject(i));
		}

		return amContainersList;
	}

	private List<JSONObject> getAMContainerInfoForAHSWebService()
			throws ClientHandlerException, UniformInterfaceException, JSONException
	{
		Client webServiceClient = Client.create();
		String webAppAddress = WebAppUtils.getHttpSchemePrefix(getConfiguration()) + WebAppUtils.getAHSWebAppURLWithoutScheme(getConfiguration());
		WebResource webResource = webServiceClient.resource(webAppAddress);

		ClientResponse response = webResource.path("ws").path("v1").path("applicationhistory").path("apps")
				.path(applicationId).path("appattempts").accept(MediaType.APPLICATION_JSON)
				.get(ClientResponse.class);

		JSONObject json = response.getEntity(JSONObject.class);
		JSONArray requests = json.getJSONArray("appAttempt");

		List<JSONObject> amContainersList = new ArrayList<JSONObject>();
		for (int i = 0; i < requests.length(); i++)
		{
			amContainersList.add(requests.getJSONObject(i));
		}

		Collections.reverse(amContainersList);

		return amContainersList;
	}


	private void printAMContainerLogs(List<String> amContainers, boolean applicationFinished)
	{
		List<JSONObject> amContainersList;
		List<AMLogsRequest> requests = new ArrayList<AMLogsRequest>();
		boolean getAMContainerLists = false;
		String errorMessage = "";

		try
		{
			amContainersList = getAMContainerInfoForRMWebService();
			if (amContainersList != null && !amContainersList.isEmpty())
			{
				getAMContainerLists = true;

				for (JSONObject amContainer : amContainersList)
				{
					AMLogsRequest request = new AMLogsRequest(applicationFinished);
					request.setAmContainerId(amContainer.getString("containerId"));
					request.setNodeHttpAddress(amContainer.getString("nodeHttpAddress"));
					request.setNodeId(amContainer.getString("nodeId"));
					requests.add(request);
				}
			}
		}
		catch (Exception ex)
		{
			errorMessage = ex.getMessage();

			if (applicationFinished)
			{
				try
				{
					amContainersList = getAMContainerInfoForAHSWebService();
					if (amContainersList != null && !amContainersList.isEmpty())
					{
						getAMContainerLists = true;
						for (JSONObject amContainer : amContainersList)
						{
							AMLogsRequest request = new AMLogsRequest(applicationFinished);
							request.setAmContainerId(amContainer.getString("amContainerId"));
							requests.add(request);
						}
					}
				}
				catch (Exception e)
				{
					errorMessage = e.getMessage();
				}
			}
		}

		if (!getAMContainerLists)
		{
			throw new BuildException("Unable to get AM container informations "
					+ "for the application: " + applicationId + SystemHelper.doubleLineSeparator + errorMessage);
		}

		if (amContainers.contains("ALL"))
		{
			for (AMLogsRequest request : requests)
			{
				outputAMContainerLogs(request);
			}

			this.log("Specified ALL for amLogs parameter. Printed logs for all am containers.");

			return;
		}

		for (String amContainer : amContainers)
		{
			int amContainerId = Integer.parseInt(amContainer.trim());
			if (amContainerId == -1)
			{
				outputAMContainerLogs(requests.get(requests.size() - 1));
			}
			else
			{
				if (amContainerId <= requests.size())
				{
					outputAMContainerLogs(requests.get(amContainerId - 1));
				}
			}
		}
	}

	private void outputAMContainerLogs(AMLogsRequest request)
	{
		String nodeHttpAddress = request.getNodeHttpAddress();
		String containerId = request.getAmContainerId();
		String nodeId = request.getNodeId();

		if (request.isAppFinished())
		{
			if (containerId != null && !containerId.isEmpty())
			{
				if (nodeId == null || nodeId.isEmpty())
				{
					try
					{
						nodeId = getContainerReport(containerId).getAssignedNode().toString();
					}
					catch (Exception e)
					{
						log(e, Project.MSG_WARN);
						nodeId = null;
					}
				}

				if (nodeId != null && !nodeId.isEmpty())
				{
					printContainerLogs(containerId, nodeId);
				}
			}

			return;
		}

		if (nodeHttpAddress != null && containerId != null &&
				!nodeHttpAddress.isEmpty() && !containerId.isEmpty())
		{
			printContainerLogs(containerId, nodeId);
		}
	}

	private void printContainerLogs(String containerId, String nodeId)
	{
		LogsHelper helper = new LogsHelper(this.getConfiguration(), logsFile, logsDir);
		helper.dumpAContainersLogsForALogType(applicationId, containerId, nodeId, appOwner, null);
	}
}
