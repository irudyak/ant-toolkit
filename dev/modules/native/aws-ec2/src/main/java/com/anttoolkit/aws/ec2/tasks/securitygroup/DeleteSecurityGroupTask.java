package com.anttoolkit.aws.ec2.tasks.securitygroup;

import com.amazonaws.services.ec2.model.*;

import org.apache.tools.ant.*;

import com.anttoolkit.aws.ec2.tasks.*;

public class DeleteSecurityGroupTask extends GenericEc2Task
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
		DeleteSecurityGroupRequest request = new DeleteSecurityGroupRequest();

		request.setGroupName(name);

		if (groupId != null && !groupId.trim().isEmpty())
		{
			request.setGroupId(groupId);
		}

		getEc2Client().deleteSecurityGroup(request);
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
