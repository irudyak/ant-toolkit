package com.anttoolkit.aws.tasks;

import com.amazonaws.*;

import org.apache.tools.ant.*;

import com.anttoolkit.aws.common.*;

public abstract class AwsClientTask extends GenericAwsTask
{
	protected abstract String serviceName();
	protected abstract Class<? extends AmazonWebServiceClient> awsClientClass();

	protected final AmazonWebServiceClient getClient()
	{
		if (!getRegion().isServiceSupported(serviceName()))
		{
			throw new BuildException("AWS service '" + serviceName() + "' is not supported in '" + getRegion() + "' region");
		}

		return AwsSessionManager.getClient(awsClientClass(), this);
	}
}
