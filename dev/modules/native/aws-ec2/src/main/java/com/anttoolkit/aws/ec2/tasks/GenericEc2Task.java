package com.anttoolkit.aws.ec2.tasks;

import com.amazonaws.*;
import com.amazonaws.regions.*;
import com.amazonaws.services.ec2.*;

import org.apache.tools.ant.*;

import com.anttoolkit.aws.tasks.*;

public abstract class GenericEc2Task extends AwsClientTask
{
	protected final AmazonEC2Client getEc2Client()
	{
		try
		{
			return (AmazonEC2Client) getClient();
		}
		catch (Throwable e)
		{
			throw new BuildException("Failed to instantiate AWS EC2 client", e);
		}
	}

	protected final String serviceName()
	{
		return ServiceAbbreviations.EC2;
	}

	protected final Class<? extends AmazonWebServiceClient> awsClientClass()
	{
		return AmazonEC2Client.class;
	}
}
