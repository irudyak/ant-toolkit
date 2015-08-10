package com.anttoolkit.hadoop.tasks.yarn;

import java.text.*;
import java.util.*;

import org.apache.commons.lang.*;
import org.apache.hadoop.yarn.api.records.*;

public abstract class NodeTask extends GenericYarnTask
{
	private String nodeIdProperty;
	private String rackProperty;
	private String stateProperty;
	private String httpAddressProperty;
	private String healthUpdateProperty;
	private String healthReportProperty;
	private String containersProperty;
	private String memoryUsedProperty;
	private String memoryCapacityProperty;
	private String cpuUsedProperty;
	private String cpuCapacityProperty;
	private String nodeLabelsProperty;

	public void setNodeIdProperty(String property)
	{
		this.nodeIdProperty = property;
	}

	public void setRackProperty(String property)
	{
		this.rackProperty = property;
	}

	public void setStateProperty(String property)
	{
		this.stateProperty = property;
	}

	public void setHttpAddressProperty(String property)
	{
		this.httpAddressProperty = property;
	}

	public void setHealthUpdateProperty(String property)
	{
		this.healthUpdateProperty = property;
	}

	public void setHealthReportProperty(String property)
	{
		this.healthReportProperty = property;
	}

	public void setContainersProperty(String property)
	{
		this.containersProperty = property;
	}

	public void setMemoryUsedProperty(String property)
	{
		this.memoryUsedProperty = property;
	}

	public void setMemoryCapacityProperty(String property)
	{
		this.memoryCapacityProperty = property;
	}

	public void setCpuUsedProperty(String property)
	{
		this.cpuUsedProperty = property;
	}

	public void setCpuCapacityProperty(String property)
	{
		this.cpuCapacityProperty = property;
	}

	public void setNodeLabelsProperty(String property)
	{
		this.nodeLabelsProperty = property;
	}

	protected void setNodeProperties(NodeReport report)
	{
		if (nodeIdProperty != null)
		{
			this.setPropertyThreadSafe(nodeIdProperty, report.getNodeId().toString());
		}

		if (rackProperty != null)
		{
			this.setPropertyThreadSafe(rackProperty, report.getRackName());
		}

		if (stateProperty != null)
		{
			this.setPropertyThreadSafe(stateProperty, report.getNodeState().toString());
		}

		if (httpAddressProperty != null)
		{
			this.setPropertyThreadSafe(httpAddressProperty, report.getHttpAddress());
		}

		if (healthUpdateProperty != null)
		{
			SimpleDateFormat formatter = new SimpleDateFormat("MM/dd/yyyy hh:mm:ss");
			this.setPropertyThreadSafe(healthUpdateProperty, formatter.format(new Date(report.getLastHealthReportTime())));
		}

		if (healthReportProperty != null)
		{
			this.setPropertyThreadSafe(healthReportProperty, report.getHealthReport());
		}

		if (containersProperty != null)
		{
			this.setPropertyThreadSafe(containersProperty, Integer.toString(report.getNumContainers()));
		}

		if (memoryUsedProperty != null)
		{
			this.setPropertyThreadSafe(memoryUsedProperty, report.getUsed() == null ? "0" : Integer.toString(report.getUsed().getMemory()));
		}

		if (memoryCapacityProperty != null)
		{
			this.setPropertyThreadSafe(memoryCapacityProperty, Integer.toString(report.getCapability().getMemory()));
		}

		if (cpuUsedProperty != null)
		{
			this.setPropertyThreadSafe(cpuUsedProperty, report.getUsed() == null ? "0" : Integer.toString(report.getUsed().getVirtualCores()));
		}

		if (cpuCapacityProperty != null)
		{
			this.setPropertyThreadSafe(cpuCapacityProperty, Integer.toString(report.getCapability().getVirtualCores()));
		}

		if (nodeLabelsProperty != null)
		{
			List<String> nodeLabelsList = new ArrayList<String>(report.getNodeLabels());
   			Collections.sort(nodeLabelsList);
			this.setPropertyThreadSafe(nodeLabelsProperty, StringUtils.join(nodeLabelsList.iterator(), ','));
		}
	}
}
