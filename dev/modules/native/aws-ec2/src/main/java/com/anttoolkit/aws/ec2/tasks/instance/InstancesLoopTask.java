package com.anttoolkit.aws.ec2.tasks.instance;

import java.util.*;

import com.amazonaws.services.ec2.model.*;

import org.apache.tools.ant.*;

import com.anttoolkit.aws.ec2.common.*;

public class InstancesLoopTask
		extends InstanceInfoTask
		implements TaskContainer
{
	private List<Filter> filters;
	private List<Task> tasks = new LinkedList<Task>();

	public void setFilters(String filters)
	{
		this.filters = FilterParser.parse(filters);
	}

	@Override
	public void addTask(Task task)
	{
		tasks.add(task);
	}

	@Override
	public void doWork() throws BuildException
	{
		DescribeInstancesRequest request = new DescribeInstancesRequest();
		if (filters != null && !filters.isEmpty())
		{
			request.setFilters(filters);
		}

		DescribeInstancesResult result;

		try
		{
			result = getEc2Client().describeInstances(request);
		}
		catch (Throwable e)
		{
			throw new BuildException("Failed to obtain information about instances", e);
		}

		if (result.getReservations() == null || result.getReservations().isEmpty())
		{
			throw new BuildException("AWS doesn't return any information about instances");
		}

		for (Reservation res : result.getReservations())
		{
			for (Instance instance : res.getInstances())
			{
				this.setPropertiesFromReservation(res);
				this.setPropertiesFromInstance(instance);

				for (Task task : tasks)
				{
					task.perform();
				}
			}
		}
	}
}
