package com.anttoolkit.maestro.tasks;

import org.apache.tools.ant.*;

import com.anttoolkit.general.tasks.*;
import com.anttoolkit.maestro.types.*;
import com.anttoolkit.maestro.common.*;

public abstract class GenericMaestroTask extends GenericTask
{
	protected static final String ACCESS_ID_PARAM = "Maestro-access-id";
	protected static final String AUTHORIZATION_PARAM = "Maestro-authorization";

	private MaestroConfig config;

	public void setMaestroConfig(String ref)
	{
		Object obj = this.getReference(ref);
		if (!(obj instanceof MaestroConfig))
		{
			throw new BuildException("Reference '" + ref + "' doesn't contain Maestro config object");
		}

		config = (MaestroConfig)obj;
	}

	@Override
	protected void preProcessing()
	{
		if (config != null)
		{
			MaestroSessionManager.setCurrentContext(config);
		}
	}

	@Override
	protected void postProcessing()
	{
		if (config != null)
		{
			MaestroSessionManager.releaseCurrentContext();
		}
	}
}
