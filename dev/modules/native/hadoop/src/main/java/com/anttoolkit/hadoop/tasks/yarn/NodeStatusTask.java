package com.anttoolkit.hadoop.tasks.yarn;

import java.util.*;

import org.apache.hadoop.yarn.api.records.*;
import org.apache.tools.ant.*;

public class NodeStatusTask extends NodeTask
{
	private String nodeId;

	public void setNodeId(String id)
	{
		this.nodeId = id;
	}

	@Override
	protected void doHadoopWork() throws BuildException
	{
		List<NodeReport> nodeReports;

		try
		{
			nodeReports = getYarnClient().getNodeReports();
		}
		catch (Throwable e)
		{
			throw new BuildException("Failed to get YARN cluster nodes reports", e);
		}

		for (NodeReport report : nodeReports)
		{
			if (report.getNodeId().toString().equals(nodeId))
			{
				setNodeProperties(report);
				return;
			}
		}

		throw new BuildException("Failed to find node: " + nodeId);
	}

	@Override
	protected void hadoopValidate()
	{
		if (nodeId == null || nodeId.trim().isEmpty())
		{
			throw new BuildException("Node id should be specified");
		}
	}
}
