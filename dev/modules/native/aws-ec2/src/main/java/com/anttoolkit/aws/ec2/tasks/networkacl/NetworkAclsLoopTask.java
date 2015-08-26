package com.anttoolkit.aws.ec2.tasks.networkacl;

import java.util.*;

import com.amazonaws.services.ec2.model.*;

import org.apache.tools.ant.*;

import com.anttoolkit.aws.ec2.common.*;
import com.anttoolkit.general.common.*;

public class NetworkAclsLoopTask
		extends NetworkAclInfoTask
		implements TaskContainer
{
	private List<Task> tasks = new LinkedList<Task>();
	private String aclIds;
	private List<Filter> filters;

	@Override
	public void doWork() throws BuildException
	{
		DescribeNetworkAclsRequest request = new DescribeNetworkAclsRequest();

		if (aclIds != null)
		{
			request.setNetworkAclIds(CollectionsHelper.asList(aclIds));
		}

		if (filters != null)
		{
			request.setFilters(filters);
		}

		DescribeNetworkAclsResult result = getEc2Client().describeNetworkAcls(request);

		for (NetworkAcl acl : result.getNetworkAcls())
		{
			setPropertiesFromNetworkAcl(acl);

			for (Task task : tasks)
			{
				task.perform();
			}
		}
	}

	public void setFilters(String filters)
	{
		this.filters = FilterParser.parse(filters);
	}

	public void setAclIds(String ids)
	{
		this.aclIds = ids;
	}

	@Override
	public void addTask(Task task)
	{
		tasks.add(task);
	}
}
