package com.anttoolkit.aws.ec2.tasks.subnet;

import com.amazonaws.services.ec2.model.*;

import org.apache.tools.ant.*;

import com.anttoolkit.general.common.annotations.*;
import com.anttoolkit.aws.ec2.tasks.*;

public class CreateSubnetTask extends GenericEc2Task
{
	@Required("VPC id should be specified")
	private String vpcId;

	@Required("CIDR block should be specified")
	private String cidrBlock;

	private String availabilityZone;
	private String name;
	private String subnetIdProperty;

	public void setVpcId(String id)
	{
		this.vpcId = id;
	}

	public void setCidrBlock(String cidr)
	{
		this.cidrBlock = cidr;
	}

	public void setAvailabilityZone(String zone)
	{
		this.availabilityZone = zone;
	}

	public void setName(String name)
	{
		this.name = name;
	}

	public void setSubnetIdProperty(String property)
	{
		this.subnetIdProperty = property;
	}

	@Override
	public void doWork() throws BuildException
	{
		CreateSubnetRequest request = new CreateSubnetRequest();
		request.setVpcId(vpcId);
		request.setCidrBlock(cidrBlock);

		String zone = availabilityZone;

		if (zone == null)
		{
			try
			{
				zone = getAvailabilityZone();
			}
			catch (Throwable e) {}
		}

		if (zone != null)
		{
			request.setAvailabilityZone(zone);
		}

		CreateSubnetResult result = getEc2Client().createSubnet(request);

		if (subnetIdProperty != null)
		{
			this.setPropertyThreadSafe(subnetIdProperty, result.getSubnet().getSubnetId());
		}

		if (name != null)
		{
			this.setResourceName(result.getSubnet().getSubnetId(), name);
		}
	}
}
