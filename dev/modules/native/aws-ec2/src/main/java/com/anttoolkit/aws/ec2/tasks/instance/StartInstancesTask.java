package com.anttoolkit.aws.ec2.tasks.instance;

import java.util.*;

import com.amazonaws.services.ec2.model.*;

import org.apache.tools.ant.*;

public class StartInstancesTask extends InstanceTask
{
	StartInstancesRequest request = new StartInstancesRequest();

	public void setInstanceIds(String ids)
	{
		if (ids == null || ids.trim().isEmpty())
		{
			throw new BuildException("Instance ids can't be empty");
		}

		String[] idsArray = ids.split(",");
		List<String> idsList = new ArrayList<String>(idsArray.length);

		for (String id : idsArray)
		{
			id = id.trim();

			if (!id.isEmpty() && !idsList.contains(id))
			{
				idsList.add(id);
			}
		}

		if (idsList.isEmpty())
		{
			throw new BuildException("Instance ids can't be empty");
		}

		request.setInstanceIds(idsList);
	}

	@Override
	public void doWork() throws BuildException
	{
		processResult(getEc2Client().startInstances(request));
	}

	@Override
	protected void validate()
	{
		if (request.getInstanceIds() == null || request.getInstanceIds().isEmpty())
		{
			throw new BuildException("Instance ids should be specified");
		}
	}

	private void processResult(StartInstancesResult result)
	{
		if (!isAsync())
		{
			waitInstancesStatusToChange(getInstanceIds(result.getStartingInstances()), RUNNING_STATUS, true);
		}
	}
}
