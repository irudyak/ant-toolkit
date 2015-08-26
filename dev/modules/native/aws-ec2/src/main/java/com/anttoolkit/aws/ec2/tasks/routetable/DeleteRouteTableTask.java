package com.anttoolkit.aws.ec2.tasks.routetable;

import com.amazonaws.services.ec2.model.*;

import org.apache.tools.ant.*;

import com.anttoolkit.general.common.annotations.*;
import com.anttoolkit.aws.ec2.tasks.*;

public class DeleteRouteTableTask extends GenericEc2Task
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
		DeleteRouteTableRequest request = new DeleteRouteTableRequest();
		request.setRouteTableId(tableId);

		getEc2Client().deleteRouteTable(request);
	}
}
