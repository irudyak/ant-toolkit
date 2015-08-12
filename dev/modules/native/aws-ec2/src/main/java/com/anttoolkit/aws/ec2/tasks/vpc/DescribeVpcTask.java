package com.anttoolkit.aws.ec2.tasks.vpc;

import java.util.*;

import com.amazonaws.services.ec2.model.*;

import org.apache.tools.ant.*;

public class DescribeVpcTask extends VpcInfoTask
{
	private String vpcId;

	public void setVpcId(String id)
	{
		vpcId = id;
	}

	@Override
	public void doWork() throws BuildException
	{
		DescribeVpcsRequest request = new DescribeVpcsRequest();
		request.setVpcIds(Arrays.asList(vpcId));

		DescribeVpcsResult result = getEc2Client().describeVpcs(request);
		if (result == null || result.getVpcs().isEmpty())
		{
			throw new BuildException("Vpc '" + vpcId + "' wasn't found in AWS");
		}

		setPropertiesFromVpc(result.getVpcs().iterator().next());
	}

	@Override
	protected void validate()
	{
		if (vpcId == null || vpcId.trim().isEmpty())
		{
			throw new BuildException("VPC id should be specified");
		}
	}
}
