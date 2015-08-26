package com.anttoolkit.aws.ec2.tasks.routetable;

import java.util.*;

import com.amazonaws.services.ec2.model.*;

import org.apache.tools.ant.*;

import com.anttoolkit.general.common.annotations.*;
import com.anttoolkit.aws.ec2.tasks.*;
import com.anttoolkit.aws.ec2.tasks.routetable.util.*;

public class CreateRouteTableTask extends GenericEc2Task
{
	@Required("VPC id should be specified")
	private String vpcId;
	private String name;
	private String tableIdProperty;
	private List<RouteWrapper> routes = new LinkedList<RouteWrapper>();

	public void addConfiguredRoute(RouteWrapper route)
	{
		routes.add(route);
	}

	@Override
	public void doWork() throws BuildException
	{
		CreateRouteTableRequest request = new CreateRouteTableRequest();
		request.setVpcId(vpcId);

		CreateRouteTableResult result = getEc2Client().createRouteTable(request);

		String routeTableId = result.getRouteTable().getRouteTableId();

		if (tableIdProperty != null)
		{
			this.setPropertyThreadSafe(tableIdProperty, routeTableId);
		}

		if (name != null)
		{
			this.setResourceName(routeTableId, name);
		}

		for (RouteWrapper route : routes)
		{
			route.createRoute(routeTableId, getEc2Client());
		}
	}
}
