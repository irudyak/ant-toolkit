package com.anttoolkit.aws.ec2.tasks.networkacl;

import java.util.*;

import com.amazonaws.services.ec2.model.*;

import org.apache.tools.ant.*;

import com.anttoolkit.general.common.annotations.*;

public class DescribeNetworkAclTask extends NetworkAclInfoTask
{
	@Required("Network ACL id should be specified")
	private String aclId;

	@Override
	public void doWork() throws BuildException
	{
		DescribeNetworkAclsRequest request = new DescribeNetworkAclsRequest();
		request.setNetworkAclIds(Arrays.asList(aclId));

		DescribeNetworkAclsResult result = getEc2Client().describeNetworkAcls(request);

		if (result == null || result.getNetworkAcls() == null || result.getNetworkAcls().isEmpty())
		{
			throw new BuildException("Network ACL with id '" + aclId + "' doesn't exist");
		}

		setPropertiesFromNetworkAcl(result.getNetworkAcls().iterator().next());
	}
}
