package com.anttoolkit.aws.ec2.tasks.vpc;

import java.util.*;

import com.amazonaws.services.ec2.model.*;

import org.apache.tools.ant.*;

import com.anttoolkit.general.common.*;
import com.anttoolkit.aws.ec2.common.*;

public class VpcsLoopTask
		extends VpcInfoTask
		implements TaskContainer
{
	private List<Task> tasks = new LinkedList<Task>();
	private String vpcIds;
	private List<Filter> filters;

	public void setFilters(String filters)
	{
		this.filters = FilterParser.parse(filters);
	}

	public void setVpcIds(String ids)
	{
		this.vpcIds = ids;
	}

	@Override
	public void addTask(Task task)
	{
		tasks.add(task);
	}

	@Override
	public void doWork() throws BuildException
	{
		DescribeVpcsRequest request = new DescribeVpcsRequest();

		if (vpcIds != null)
		{
			request.setVpcIds(CollectionsHelper.asList(vpcIds));
		}

		if (filters != null)
		{
			request.setFilters(filters);
		}

		DescribeVpcsResult result = getEc2Client().describeVpcs(request);

		for (Vpc vpc : result.getVpcs())
		{
			setPropertiesFromVpc(vpc);

			for (Task task : tasks)
			{
				task.perform();
			}
		}

	}
}
