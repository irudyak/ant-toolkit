package com.anttoolkit.aws.ec2.tasks.dhcp;

import java.util.*;

import com.amazonaws.services.ec2.model.*;

import org.apache.tools.ant.*;

import com.anttoolkit.general.common.*;
import com.anttoolkit.aws.ec2.common.*;

public class DhcpOptionsLoopTask
		extends DhcpOptionsInfoTask
		implements TaskContainer
{
	private List<Task> tasks = new LinkedList<Task>();
	private String optionsIds;
	private List<Filter> filters;

	public void setFilters(String filters)
	{
		this.filters = FilterParser.parse(filters);
	}

	public void setOptionsIds(String ids)
	{
		this.optionsIds = ids;
	}

	@Override
	public void addTask(Task task)
	{
		tasks.add(task);
	}

	@Override
	public void doWork() throws BuildException
	{
		DescribeDhcpOptionsRequest request = new DescribeDhcpOptionsRequest();

		if (optionsIds != null)
		{
			request.setDhcpOptionsIds(CollectionsHelper.asList(optionsIds));
		}

		if (filters != null)
		{
			request.setFilters(filters);
		}

		DescribeDhcpOptionsResult result = getEc2Client().describeDhcpOptions(request);

		for (DhcpOptions opts : result.getDhcpOptions())
		{
			setPropertiesFromDhcpOptions(opts);

			for (Task task : tasks)
			{
				task.perform();
			}
		}
	}
}
