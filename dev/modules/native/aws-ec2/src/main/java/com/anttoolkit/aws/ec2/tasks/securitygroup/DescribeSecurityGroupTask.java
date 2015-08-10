package com.anttoolkit.aws.ec2.tasks.securitygroup;

import java.util.*;

import com.amazonaws.services.ec2.model.*;

import org.apache.tools.ant.*;

public class DescribeSecurityGroupTask extends SecurityGroupInfoTask
{
	private String name;
	private String groupId;

	public void setName(String name)
	{
		this.name = name;
	}

	public void setGroupId(String id)
	{
		this.groupId = id;
	}

	@Override
	public void doWork() throws BuildException
	{
		DescribeSecurityGroupsRequest request = new DescribeSecurityGroupsRequest();

		if (name != null)
		{
			request.setGroupNames(Arrays.asList(name));
		}

		if (groupId != null)
		{
			request.setGroupIds(Arrays.asList(groupId));
		}

		DescribeSecurityGroupsResult result = getEc2Client().describeSecurityGroups(request);

		if (result == null || result.getSecurityGroups() == null || result.getSecurityGroups().isEmpty())
		{
			throw new BuildException("No security groups found");
		}

		setPropertiesFromGroup(result.getSecurityGroups().iterator().next());
	}

	@Override
	protected void validate()
	{
		if ((name == null || name.trim().isEmpty()) && (groupId == null || groupId.trim().isEmpty()))
		{
			throw new BuildException("Group name or id should be specified for security group");
		}
	}
}
