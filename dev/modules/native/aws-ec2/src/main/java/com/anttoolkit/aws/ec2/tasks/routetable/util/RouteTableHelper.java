package com.anttoolkit.aws.ec2.tasks.routetable.util;

import java.util.*;

import com.amazonaws.services.ec2.*;
import com.amazonaws.services.ec2.model.*;

import org.apache.tools.ant.*;

public class RouteTableHelper
{
	public static String findAssociation(AmazonEC2Client client, String tableId, String subnetId)
	{
		DescribeRouteTablesRequest describeRequest = new DescribeRouteTablesRequest();
		describeRequest.setRouteTableIds(Arrays.asList(tableId));

		DescribeRouteTablesResult result = client.describeRouteTables(describeRequest);
		if (result == null || result.getRouteTables() == null || result.getRouteTables().isEmpty())
		{
			throw new BuildException("Failed to find route table with id '" + tableId + "'");
		}

		List<RouteTableAssociation> associations = result.getRouteTables().iterator().next().getAssociations();
		for (RouteTableAssociation association : associations)
		{
			if (association.getSubnetId().equals(subnetId))
			{
				return association.getRouteTableAssociationId();
			}
		}

		throw new BuildException("Failed to find route table '" + tableId + "' association with subnet '" + subnetId + "'");
	}
}
