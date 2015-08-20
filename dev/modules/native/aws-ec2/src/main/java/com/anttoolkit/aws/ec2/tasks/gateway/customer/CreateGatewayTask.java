package com.anttoolkit.aws.ec2.tasks.gateway.customer;

import com.amazonaws.services.ec2.model.*;

import org.apache.tools.ant.*;

import com.anttoolkit.general.common.annotations.*;
import com.anttoolkit.aws.ec2.tasks.*;

public class CreateGatewayTask extends GenericEc2Task
{
	private String name;
	private String gatewayIdProperty;

	@Required("IP address should be specified")
	private String ip;
	private String type;
	private Integer bgpAsn;

	public void setName(String name)
	{
		this.name = name;
	}

	public void setGatewayIdProperty(String property)
	{
		gatewayIdProperty = property;
	}

	public void setType(String type)
	{
		this.type = type;
	}

	public void setIp(String ip)
	{
		this.ip = ip;
	}

	public void setBgpAsn(int bgpAsn)
	{
		this.bgpAsn = bgpAsn;
	}

	@Override
	public void doWork() throws BuildException
	{
		CreateCustomerGatewayRequest request = new CreateCustomerGatewayRequest();

		request.setPublicIp(ip);

		if (type != null)
		{
			request.setType(type);
		}

		if (bgpAsn != null)
		{
			request.setBgpAsn(bgpAsn);
		}

		CreateCustomerGatewayResult result = getEc2Client().createCustomerGateway(request);

		if (gatewayIdProperty != null)
		{
			this.setPropertyThreadSafe(gatewayIdProperty, result.getCustomerGateway().getCustomerGatewayId());
		}

		if (name != null)
		{
			this.setResourceName(result.getCustomerGateway().getCustomerGatewayId(), name);
		}
	}
}
