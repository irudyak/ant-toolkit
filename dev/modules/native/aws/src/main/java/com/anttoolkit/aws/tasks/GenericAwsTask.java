package com.anttoolkit.aws.tasks;

import com.amazonaws.regions.*;

import org.apache.tools.ant.*;

import com.anttoolkit.general.tasks.*;
import com.anttoolkit.aws.types.*;
import com.anttoolkit.aws.common.*;

public abstract class GenericAwsTask extends GenericTask
{
	private AwsConfig config;

	public void setAwsConfig(String ref)
	{
		Object obj = this.getReference(ref);
		if (!(obj instanceof AwsConfig))
		{
			throw new BuildException("Reference '" + ref + "' doesn't contain Aws config object");
		}

		config = (AwsConfig)obj;
	}

	@Override
	protected final void preProcessing()
	{
		if (config != null)
		{
			AwsSessionManager.setCurrentContext(config);
		}
	}

	@Override
	protected void postProcessing()
	{
		if (config != null)
		{
			AwsSessionManager.releaseCurrentContext();
		}
	}

	protected final Region getRegion()
	{
		return AwsSessionManager.getRegion();
	}

	protected final String getAvailabilityZone()
	{
		return AwsSessionManager.getAvailabilityZone();
	}
}
