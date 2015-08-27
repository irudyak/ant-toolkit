package com.anttoolkit.aws.ec2.tasks.routetable;

import com.amazonaws.services.ec2.model.*;

import org.apache.tools.ant.*;

import com.anttoolkit.aws.ec2.tasks.*;
import com.anttoolkit.aws.ec2.tasks.routetable.util.*;

public class DisassociateRouteTableTask extends GenericEc2Task
{
	private String associationId;
	private String subnetId;
	private String tableId;

	public void setAssociationId(String id)
	{
		associationId = id;
	}

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
		DisassociateRouteTableRequest request = new DisassociateRouteTableRequest();
		request.setAssociationId(associationId != null ?
				associationId :
				RouteTableHelper.findAssociation(getEc2Client(), tableId, subnetId));

		getEc2Client().disassociateRouteTable(request);
	}

	@Override
	protected void validate()
	{
		if (associationId == null && (subnetId == null || tableId == null))
		{
			throw new BuildException("Either association id or subnet and route table id should be specified");
		}
	}
}
