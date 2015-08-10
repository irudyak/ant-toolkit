package com.anttoolkit.aws.ec2.tasks.instance;

import java.util.*;

import com.amazonaws.services.ec2.model.*;

import org.apache.tools.ant.*;

public class StopInstancesTask extends InstanceTask
{
	StopInstancesRequest request = new StopInstancesRequest();

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

	public void setForce(boolean force)
	{
		request.setForce(force);
	}

	@Override
	public void doWork() throws BuildException
	{
		processResult(getEc2Client().stopInstances(request));
	}

	@Override
	protected void validate()
	{
		if (request.getInstanceIds() == null || request.getInstanceIds().isEmpty())
		{
			throw new BuildException("Instance ids should be specified");
		}
	}

	private void processResult(StopInstancesResult result)
	{
		if (!isAsync())
		{
			waitInstancesStatusToChange(getInstanceIds(result.getStoppingInstances()), STOPPED_STATUS, true);
		}
	}
}
