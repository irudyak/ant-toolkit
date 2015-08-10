package com.anttoolkit.aws.ec2.tasks.securitygroup;

import java.util.*;

import com.amazonaws.services.ec2.model.*;

import org.apache.tools.ant.*;

import com.anttoolkit.aws.ec2.common.*;
import com.anttoolkit.general.common.*;

public class SecurityGroupsLoopTask
		extends SecurityGroupInfoTask
		implements TaskContainer
{
	private String names;
	private String groupIds;
	private List<Filter> filters;
	private List<Task> tasks = new LinkedList<Task>();

	public void setFilters(String filters)
	{
		this.filters = FilterParser.parse(filters);
	}

	public void setNames(String names)
	{
		this.names = names;
	}

	public void setGroupIds(String ids)
	{
		this.groupIds = ids;
	}

	@Override
	public void addTask(Task task)
	{
		tasks.add(task);
	}

	@Override
	public void doWork() throws BuildException
	{
		DescribeSecurityGroupsRequest request = new DescribeSecurityGroupsRequest();

		if (names != null)
		{
			request.setGroupNames(CollectionsHelper.asList(names));
		}

		if (groupIds != null)
		{
			request.setGroupIds(CollectionsHelper.asList(groupIds));
		}

		if (filters != null)
		{
			request.setFilters(filters);
		}

		DescribeSecurityGroupsResult result = getEc2Client().describeSecurityGroups(request);

		for (SecurityGroup group : result.getSecurityGroups())
		{
			setPropertiesFromGroup(group);

			for (Task task : tasks)
			{
				task.perform();
			}
		}
	}
}
