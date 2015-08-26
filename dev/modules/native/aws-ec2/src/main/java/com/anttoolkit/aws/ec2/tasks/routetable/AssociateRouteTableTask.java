package com.anttoolkit.aws.ec2.tasks.routetable;

import com.amazonaws.services.ec2.model.AssociateRouteTableRequest;
import org.apache.tools.ant.*;

import com.anttoolkit.general.common.annotations.*;
import com.anttoolkit.aws.ec2.tasks.*;

public class AssociateRouteTableTask extends GenericEc2Task
{
	@Required("Subnet id should be specified")
	private String subnetId;

	@Required("Route table id should be specified")
	private String tableId;

	public void setSubnetId(String id)
	{
		subnetId = id;
	}

	public void setTableId(String id)
	{
		tableId = id;
	}

	@Override
	public void doWork() throws BuildException
	{
		AssociateRouteTableRequest request = new AssociateRouteTableRequest();
		request.setSubnetId(subnetId);
		request.setRouteTableId(tableId);

		getEc2Client().associateRouteTable(request);
	}
}
