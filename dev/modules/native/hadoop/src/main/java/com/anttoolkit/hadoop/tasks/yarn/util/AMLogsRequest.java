package com.anttoolkit.hadoop.tasks.yarn.util;

public class AMLogsRequest
{
	private String amContainerId;
	private String nodeId;
	private String nodeHttpAddress;
	private final boolean isAppFinished;

	public AMLogsRequest(boolean isAppFinished)
	{
		this.isAppFinished = isAppFinished;
		this.setAmContainerId("");
		this.setNodeId("");
		this.setNodeHttpAddress("");
	}

	public String getAmContainerId()
	{
		return amContainerId;
	}

	public void setAmContainerId(String amContainerId)
	{
		this.amContainerId = amContainerId;
	}

	public String getNodeId()
	{
		return nodeId;
	}

	public void setNodeId(String nodeId)
	{
		this.nodeId = nodeId;
	}

	public String getNodeHttpAddress()
	{
		return nodeHttpAddress;
	}

	public void setNodeHttpAddress(String nodeHttpAddress)
	{
		this.nodeHttpAddress = nodeHttpAddress;
	}

	public boolean isAppFinished()
	{
		return isAppFinished;
	}
}
