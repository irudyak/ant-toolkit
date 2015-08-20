package com.anttoolkit.aws.ec2.tasks.gateway.customer;

import com.amazonaws.services.ec2.model.*;

import org.apache.tools.ant.*;

import com.anttoolkit.aws.ec2.tasks.*;
import com.anttoolkit.general.common.annotations.*;

public class DeleteGatewayTask extends GenericEc2Task
{
	@Required("Gateway id should be specified")
	private String gatewayId;

	public void setGatewayId(String id)
	{
		gatewayId = id;
	}

	@Override
	public void doWork() throws BuildException
	{
		getEc2Client().deleteCustomerGateway((new DeleteCustomerGatewayRequest(gatewayId)));
	}
}
