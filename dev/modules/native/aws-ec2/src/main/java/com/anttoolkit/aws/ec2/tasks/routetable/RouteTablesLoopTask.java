package com.anttoolkit.aws.ec2.tasks.routetable;

import java.util.*;

import com.amazonaws.services.ec2.model.*;

import org.apache.tools.ant.*;

import com.anttoolkit.general.common.*;
import com.anttoolkit.aws.ec2.common.*;

public class RouteTablesLoopTask
		extends RouteTableInfoTask
		implements TaskContainer
{
	private String tableIds;
	private List<Filter> filters;
	private List<Task> tasks = new LinkedList<Task>();

	public void setFilters(String filters)
	{
		this.filters = FilterParser.parse(filters);
	}

	public void setTableIds(String ids)
	{
		this.tableIds = ids;
	}

	@Override
	public void addTask(Task task)
	{
		tasks.add(task);
	}

	@Override
	public void doWork() throws BuildException
	{
		DescribeRouteTablesRequest request = new DescribeRouteTablesRequest();

		if (tableIds != null)
		{
			request.setRouteTableIds(CollectionsHelper.asList(tableIds));
		}

		if (filters != null)
		{
			request.setFilters(filters);
		}

		DescribeRouteTablesResult result = getEc2Client().describeRouteTables(request);

		for (RouteTable table : result.getRouteTables())
		{
			setPropertiesFromRouteTable(table);

			for (Task task : tasks)
			{
				task.perform();
			}
		}
	}
}
