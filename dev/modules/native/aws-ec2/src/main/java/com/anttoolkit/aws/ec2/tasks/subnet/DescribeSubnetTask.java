package com.anttoolkit.aws.ec2.tasks.subnet;

import java.util.*;

import com.amazonaws.services.ec2.model.*;

import org.apache.tools.ant.*;

import com.anttoolkit.general.common.annotations.*;

public class DescribeSubnetTask extends SubnetInfoTask
{
	@Required("Subnet id should be specified")
	private String subnetId;

	public void setSubnetId(String id)
	{
		subnetId = id;
	}

	@Override
	public void doWork() throws BuildException
	{
		DescribeSubnetsRequest request = new DescribeSubnetsRequest();
		request.setSubnetIds(Arrays.asList(subnetId));

		DescribeSubnetsResult result = getEc2Client().describeSubnets(request);
		if (result == null || result.getSubnets() == null || result.getSubnets().isEmpty())
		{
			throw new BuildException("Subnet '" + subnetId + "' doesn't exist");
		}

		this.setPropertiesFromSubnet(result.getSubnets().iterator().next());
	}
}
