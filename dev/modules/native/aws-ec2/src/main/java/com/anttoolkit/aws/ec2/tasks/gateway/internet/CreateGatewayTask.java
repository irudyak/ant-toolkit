package com.anttoolkit.aws.ec2.tasks.gateway.internet;

import com.amazonaws.services.ec2.model.*;

import org.apache.tools.ant.*;

import com.anttoolkit.aws.ec2.tasks.*;

public class CreateGatewayTask extends GenericEc2Task
{
	private String name;
	private String gatewayIdProperty;

	public void setName(String name)
	{
		this.name = name;
	}

	public void setGatewayIdProperty(String property)
	{
		gatewayIdProperty = property;
	}

	@Override
	public void doWork() throws BuildException
	{
		CreateInternetGatewayResult result = getEc2Client().createInternetGateway();

		if (gatewayIdProperty != null)
		{
			this.setPropertyThreadSafe(gatewayIdProperty, result.getInternetGateway().getInternetGatewayId());
		}

		if (name != null)
		{
			this.setResourceName(result.getInternetGateway().getInternetGatewayId(), name);
		}
	}
}
