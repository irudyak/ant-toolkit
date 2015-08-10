package com.anttoolkit.aws.ec2.tasks.securitygroup;

import java.util.*;

import com.amazonaws.services.ec2.*;
import com.amazonaws.services.ec2.model.*;

import com.anttoolkit.general.tasks.strings.util.StringToken;
import org.apache.tools.ant.*;

import com.anttoolkit.aws.ec2.tasks.*;
import com.anttoolkit.aws.ec2.tasks.securitygroup.util.*;

public class CreateSecurityGroupTask extends GenericEc2Task
{
	private String name;
	private String description;
	private String vpcId;
	private String groupIdProperty;
	private List<Authorize> authorizations = new LinkedList<Authorize>();

	public void setName(String name)
	{
		this.name = name;
	}

	public void setDescription(String description)
	{
		this.description = description;
	}

	public void setVpcId(String id)
	{
		this.vpcId = id;
	}

	public void setGroupIdProperty(String property)
	{
		this.groupIdProperty = property;
	}

	public void addConfiguredAuthorize(Authorize auth)
	{
		authorizations.add(auth);
	}

	@Override
	public void doWork() throws BuildException
	{
		CreateSecurityGroupRequest request = new CreateSecurityGroupRequest(name.trim(), description.trim());
		if (vpcId != null && !vpcId.trim().isEmpty())
		{
			request.setVpcId(vpcId);
		}

		AmazonEC2Client client = getEc2Client();
		CreateSecurityGroupResult result;

		try
		{
			result = client.createSecurityGroup(request);
		}
		catch (Throwable e)
		{
			throw new BuildException("Failed to create security group '" + name + "'");
		}

		if (groupIdProperty != null)
		{
			this.setPropertyThreadSafe(groupIdProperty, result.getGroupId());
		}

		for (Authorize auth : authorizations)
		{
			auth.authorize(name, result.getGroupId(), client);
		}
	}

	@Override
	protected void validate()
	{
		if (name == null || name.trim().isEmpty())
		{
			throw new BuildException("Group name should be specified for security group");
		}
	}
}
