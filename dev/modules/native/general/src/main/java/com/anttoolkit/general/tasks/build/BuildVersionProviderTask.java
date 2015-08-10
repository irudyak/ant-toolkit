package com.anttoolkit.general.tasks.build;

import java.util.*;

import org.apache.tools.ant.*;

import com.anttoolkit.general.common.*;
import com.anttoolkit.general.tasks.*;
import com.anttoolkit.general.tasks.build.util.*;

public class BuildVersionProviderTask extends GenericTask
{
	private static final String BUILD_QUALIFIER_PROPERTY = "build.qualifier";

	private Map<String, String> properties = new HashMap<String, String>();
	private String buildQualifier;
	private String versionProviderClass;

	public void addConfiguredProperty(NameValueHolder prop)
	{
		if (!BUILD_QUALIFIER_PROPERTY.equals(prop.getName()))
		{
			properties.put(prop.getName(), prop.getValue());
		}
		else
		{
			buildQualifier = prop.getValue();
		}
	}

	public void setVersionProviderClass(String clazz)
	{
		versionProviderClass = clazz;
	}

	@Override
	public void doWork() throws BuildException
	{
		Object obj;

		try
		{
			obj = ReflectionHelper.newInstance(getVersionProviderClass());
		}
		catch (Throwable e)
		{
			throw new BuildException("Failed to create instance of version provider class '" + getVersionProviderClass() + "'", e);
		}

		if (obj == null)
		{
			throw new BuildException("Failed to create instance of version provider class '" + getVersionProviderClass() + "'");
		}

		if (!(obj instanceof IBuildVersionProvider))
		{
			throw new BuildException("Incorrect version provider class '" + getVersionProviderClass() + "' specified");
		}

		IBuildVersionProvider provider = (IBuildVersionProvider)obj;
		provider.init(this, properties);
		provider.validate(this);

		BuildContextManager.registerVersionProvider(this, provider);

		if (buildQualifier != null)
		{
			BuildContextManager.setContextQualifier(buildQualifier);
		}
	}

	protected String getVersionProviderClass()
	{
		return versionProviderClass;
	}
}
