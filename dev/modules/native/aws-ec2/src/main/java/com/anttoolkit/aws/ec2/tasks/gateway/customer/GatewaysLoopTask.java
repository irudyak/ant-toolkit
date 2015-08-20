package com.anttoolkit.aws.ec2.tasks.gateway.customer;

import java.util.*;

import com.amazonaws.services.ec2.model.*;

import org.apache.tools.ant.*;

import com.anttoolkit.aws.ec2.common.*;
import com.anttoolkit.aws.ec2.tasks.*;
import com.anttoolkit.general.common.*;

public class GatewaysLoopTask
		extends GenericEc2Task
		implements TaskContainer
{
	private List<Task> tasks = new LinkedList<Task>();
	private String gatewayIds;
	private List<Filter> filters;
	private String property;

	public void setGatewayIds(String ids)
	{
		gatewayIds = ids;
	}

	public void setFilters(String filters)
	{
		this.filters = FilterParser.parse(filters);
	}

	public void setProperty(String property)
	{
		this.property = property;
	}

	@Override
	public void addTask(Task task)
	{
		tasks.add(task);
	}

	@Override
	public void doWork() throws BuildException
	{
		DescribeCustomerGatewaysRequest request = new DescribeCustomerGatewaysRequest();

		if (gatewayIds != null)
		{
			request.setCustomerGatewayIds(CollectionsHelper.asList(gatewayIds));
		}

		if (filters != null)
		{
			request.setFilters(filters);
		}

		DescribeCustomerGatewaysResult result = getEc2Client().describeCustomerGateways(request);

		for (CustomerGateway gateway : result.getCustomerGateways())
		{
			this.setPropertyThreadSafe(property, gateway.toString());

			for (Task task : tasks)
			{
				task.perform();
			}
		}
	}
}
