package com.anttoolkit.aws.ec2.tasks.routetable;

import com.amazonaws.services.ec2.model.*;

import org.apache.tools.ant.*;

import com.anttoolkit.general.common.annotations.*;
import com.anttoolkit.aws.ec2.tasks.*;
import com.anttoolkit.aws.ec2.tasks.routetable.util.*;

public class ReplaceRouteTableAssociationTask extends GenericEc2Task
{
	private String associationId;
	private String subnetId;
	private String tableId;

	@Required("New route table id should be specified")
	private String newTableId;

	private String newAssociationIdProperty;

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

	public void setNewTableId(String id)
	{
		newTableId = id;
	}

	public void setNewAssociationIdProperty(String property)
	{
		newAssociationIdProperty = property;
	}

	@Override
	public void doWork() throws BuildException
	{
		ReplaceRouteTableAssociationRequest request = new ReplaceRouteTableAssociationRequest();
		request.setRouteTableId(newTableId);
		request.setAssociationId(associationId != null ?
				associationId :
				RouteTableHelper.findAssociation(getEc2Client(), tableId, subnetId));

		ReplaceRouteTableAssociationResult result = getEc2Client().replaceRouteTableAssociation(request);

		if (newAssociationIdProperty != null)
		{
			this.setPropertyThreadSafe(newAssociationIdProperty, result.getNewAssociationId());
		}
	}

	@Override
	protected void validate()
	{
		if (associationId == null && (subnetId == null || tableId == null))
		{
			throw new BuildException("Either association id or subnet and route table id should be specified");
		}

		if (associationId != null && (subnetId != null || tableId != null))
		{
			throw new BuildException("Either association id or subnet and route table id should be specified");
		}
	}
}
