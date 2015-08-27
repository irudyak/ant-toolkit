package com.anttoolkit.aws.ec2.tasks.routetable;

import com.amazonaws.services.ec2.model.*;

import org.apache.tools.ant.*;

import com.anttoolkit.general.common.annotations.*;
import com.anttoolkit.aws.ec2.tasks.*;

public class AssociateRouteTableTask extends GenericEc2Task
{
	@Required("Subnet id should be specified")
	private String subnetId;

	@Required("Route table id should be specified")
	private String tableId;

	private String associationIdProperty;

	public void setSubnetId(String id)
	{
		subnetId = id;
	}

	public void setTableId(String id)
	{
		tableId = id;
	}

	public void setAssociationIdProperty(String property)
	{
		associationIdProperty = property;
	}

	@Override
	public void doWork() throws BuildException
	{
		AssociateRouteTableRequest request = new AssociateRouteTableRequest();
		request.setSubnetId(subnetId);
		request.setRouteTableId(tableId);

		AssociateRouteTableResult result = getEc2Client().associateRouteTable(request);

		if (associationIdProperty != null)
		{
			this.setPropertyThreadSafe(associationIdProperty, result.getAssociationId());
		}
	}
}
