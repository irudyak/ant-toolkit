package com.anttoolkit.aws.ec2.tasks.gateway.vpn;

import com.amazonaws.services.ec2.model.*;

import org.apache.tools.ant.*;

import com.anttoolkit.general.common.annotations.*;
import com.anttoolkit.aws.ec2.tasks.*;

public class AttachGatewayTask extends GenericEc2Task
{
	@Required("VPC id should be specified")
	private String vpcId;

	@Required("Gateway id should be specified")
	private String gatewayId;

	public void setVpcId(String id)
	{
		vpcId = id;
	}

	public void setGatewayId(String id)
	{
		gatewayId = id;
	}

	@Override
	public void doWork() throws BuildException
	{
		AttachVpnGatewayRequest request = new AttachVpnGatewayRequest();
		request.setVpnGatewayId(gatewayId);
		request.setVpcId(vpcId);

		getEc2Client().attachVpnGateway(request);
	}
}
