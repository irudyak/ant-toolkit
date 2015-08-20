package com.anttoolkit.aws.ec2.tasks.gateway.vpn;

import com.amazonaws.services.ec2.model.*;

import org.apache.tools.ant.*;

import com.anttoolkit.aws.ec2.tasks.*;

public class CreateGatewayTask extends GenericEc2Task
{
	private String name;
	private String type;
	private String availabilityZone;
	private String gatewayIdProperty;

	public void setName(String name)
	{
		this.name = name;
	}

	public void setType(String type)
	{
		this.type = type;
	}

	public void setAvailabilityZone(String zone)
	{
		this.availabilityZone = zone;
	}

	public void setGatewayIdProperty(String property)
	{
		gatewayIdProperty = property;
	}

	@Override
	public void doWork() throws BuildException
	{
		CreateVpnGatewayRequest request = new CreateVpnGatewayRequest();

		if (type != null)
		{
			request.setType(type);
		}

		if (availabilityZone != null)
		{
			request.setAvailabilityZone(availabilityZone);
		}

		CreateVpnGatewayResult result = getEc2Client().createVpnGateway(request);

		if (gatewayIdProperty != null)
		{
			this.setPropertyThreadSafe(gatewayIdProperty, result.getVpnGateway().getVpnGatewayId());
		}

		if (name != null)
		{
			this.setResourceName(result.getVpnGateway().getVpnGatewayId(), name);
		}
	}
}
