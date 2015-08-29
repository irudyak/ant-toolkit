package com.anttoolkit.aws.ec2.tasks.subnet;

import java.util.*;

import com.amazonaws.services.ec2.model.*;

import org.apache.tools.ant.*;

import com.anttoolkit.general.common.*;
import com.anttoolkit.aws.ec2.common.*;

public class SubnetsLoopTask
		extends SubnetInfoTask
		implements TaskContainer
{
	private List<Task> tasks = new LinkedList<Task>();
	private String subnetIds;
	private List<Filter> filters;

	public void setFilters(String filters)
	{
		this.filters = FilterParser.parse(filters);
	}

	public void setSubnetIds(String ids)
	{
		this.subnetIds = ids;
	}

	@Override
	public void addTask(Task task)
	{
		tasks.add(task);
	}

	@Override
	public void doWork() throws BuildException
	{
		DescribeSubnetsRequest request = new DescribeSubnetsRequest();

		if (subnetIds != null)
		{
			request.setSubnetIds(CollectionsHelper.asList(subnetIds));
		}

		if (filters != null)
		{
			request.setFilters(filters);
		}

		DescribeSubnetsResult result = getEc2Client().describeSubnets(request);

		for (Subnet subnet : result.getSubnets())
		{
			setPropertiesFromSubnet(subnet);

			for (Task task : tasks)
			{
				task.perform();
			}
		}

	}
}
