package com.anttoolkit.aws.ec2.tasks.instance;

import java.util.*;

import com.amazonaws.services.ec2.model.*;

import org.apache.tools.ant.*;

public class DescribeInstanceTask extends InstanceInfoTask
{
	private String id;

	public void setInstanceId(String id)
	{
		this.id = id;
	}

	@Override
	public void doWork() throws BuildException
	{
		DescribeInstancesRequest request = new DescribeInstancesRequest();
		request.setInstanceIds(Arrays.asList(id));

		DescribeInstancesResult result;

		try
		{
			result = getEc2Client().describeInstances(request);
		}
		catch (Throwable e)
		{
			throw new BuildException("Failed to obtain information about instance: " + id, e);
		}

		if (result.getReservations() == null || result.getReservations().isEmpty())
		{
			throw new BuildException("AWS doesn't return any information about instance: " + id);
		}

		Reservation res = result.getReservations().iterator().next();

		this.setPropertiesFromReservation(res);
		this.setPropertiesFromInstance(res.getInstances().iterator().next());
	}

	@Override
	protected void validate()
	{
		if (id.isEmpty())
		{
			throw new BuildException("Instance id should be specified");
		}
	}
}
