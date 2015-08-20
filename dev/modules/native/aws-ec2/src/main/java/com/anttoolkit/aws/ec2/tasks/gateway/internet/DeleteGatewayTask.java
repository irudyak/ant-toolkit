package com.anttoolkit.aws.ec2.tasks.gateway.internet;

import com.amazonaws.services.ec2.model.*;

import org.apache.tools.ant.*;

import com.anttoolkit.aws.ec2.tasks.*;

public class DeleteGatewayTask extends GenericEc2Task
{
	private String gatewayId;

	public void setGatewayId(String id)
	{
		gatewayId = id;
	}

	@Override
	public void doWork() throws BuildException
	{
		getEc2Client().deleteInternetGateway((new DeleteInternetGatewayRequest()).withInternetGatewayId(gatewayId));
	}

	@Override
	protected void validate()
	{
		if (gatewayId == null)
		{
			throw new BuildException("Gateway id should be specified");
		}
	}
}
