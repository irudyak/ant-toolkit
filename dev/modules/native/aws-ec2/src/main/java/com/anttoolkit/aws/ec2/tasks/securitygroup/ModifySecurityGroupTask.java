package com.anttoolkit.aws.ec2.tasks.securitygroup;

import java.util.*;

import com.amazonaws.services.ec2.*;

import org.apache.tools.ant.*;

import com.anttoolkit.aws.ec2.tasks.*;
import com.anttoolkit.aws.ec2.tasks.securitygroup.util.*;

public class ModifySecurityGroupTask  extends GenericEc2Task
{
	private String name;
	private String groupId;
	private List<Authorize> authorizations = new LinkedList<Authorize>();
	private List<Revoke> revocations = new LinkedList<Revoke>();

	public void setName(String name)
	{
		this.name = name;
	}

	public void setGroupId(String id)
	{
		this.groupId = id;
	}

	public void addConfiguredAuthorize(Authorize auth)
	{
		authorizations.add(auth);
	}

	public void addConfiguredRevoke(Revoke revoke)
	{
		revocations.add(revoke);
	}

	@Override
	public void doWork() throws BuildException
	{
		AmazonEC2Client client = getEc2Client();

		for (Authorize auth : authorizations)
		{
			auth.authorize(name, groupId, client);
		}

		for (Revoke revoke : revocations)
		{
			revoke.revoke(name, groupId, client);
		}
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
