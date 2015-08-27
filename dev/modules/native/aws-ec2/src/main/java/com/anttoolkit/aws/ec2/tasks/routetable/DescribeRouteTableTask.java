package com.anttoolkit.aws.ec2.tasks.routetable;

import java.util.*;

import com.amazonaws.services.ec2.model.*;

import org.apache.tools.ant.*;

import com.anttoolkit.general.common.annotations.*;

public class DescribeRouteTableTask extends RouteTableInfoTask
{
	@Required("Route table id should be specified")
	private String tableId;

	public void setTableId(String id)
	{
		tableId = id;
	}

	@Override
	public void doWork() throws BuildException
	{
		DescribeRouteTablesRequest describeRequest = new DescribeRouteTablesRequest();
		describeRequest.setRouteTableIds(Arrays.asList(tableId));

		DescribeRouteTablesResult result = getEc2Client().describeRouteTables(describeRequest);
		if (result == null || result.getRouteTables() == null || result.getRouteTables().isEmpty())
		{
			throw new BuildException("Failed to find route table with id '" + tableId + "'");
		}

		setPropertiesFromRouteTable(result.getRouteTables().iterator().next());
	}
}
