package com.anttoolkit.hadoop.tasks.yarn;

import java.util.*;

import org.apache.hadoop.yarn.api.records.*;
import org.apache.tools.ant.*;

public class NodesListLoopTask
		extends NodeTask
		implements TaskContainer

{
	private ArrayList<NodeState> nodeStateFilter = new ArrayList<NodeState>();

	private List<Task> tasks = new LinkedList<Task>();

	public void setNodeStateFilter(String filter)
	{
		nodeStateFilter.clear();

		if (filter == null || filter.trim().isEmpty())
		{
			return;
		}

		String[] states = filter.split(",");
		for (String state : states)
		{
			state = state.trim().toUpperCase();
			if (state.isEmpty())
			{
				continue;
			}

			try
			{
				nodeStateFilter.add(NodeState.valueOf(state));
			}
			catch (IllegalArgumentException e)
			{
				throw new BuildException("Invalid node state specified: " + state, e);
			}
		}
	}

	@Override
	public void addTask(Task task)
	{
		tasks.add(task);
	}

	@Override
	protected void doHadoopWork() throws BuildException
	{
		List<NodeReport> nodeReports;

		try
		{
			nodeReports = getYarnClient().getNodeReports(nodeStateFilter.toArray(new NodeState[0]));
		}
		catch (Throwable e)
		{
			throw new BuildException("Failed to get YARN cluster nodes reports", e);
		}

		for (NodeReport report : nodeReports)
		{
			setNodeProperties(report);

			for (Task task : tasks)
			{
				task.perform();
			}
		}
	}
}
