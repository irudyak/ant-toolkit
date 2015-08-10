package com.anttoolkit.ldap.tasks;

import javax.naming.directory.*;

import org.apache.tools.ant.*;

import com.anttoolkit.general.tasks.*;
import com.anttoolkit.ldap.types.*;
import com.anttoolkit.ldap.common.*;

public abstract class GenericLdapTask extends GenericTask
{
	private LdapConfig config;

	public void setLdapConfig(String ref)
	{
		Object obj = getReference(ref);
		if (!(obj instanceof LdapConfig))
		{
			throw new IllegalArgumentException("Incorrect LDAP config specified");
		}

		this.config = (LdapConfig)obj;

		if (config.getProperties().isEmpty())
		{
			throw new BuildException("Invalid LDAP config '" + config + "', no settings specified.");
		}
	}

	@Override
	protected void preProcessing()
	{
		if (config != null)
		{
			LdapSessionManager.setCurrentContext(config);
		}
	}

	@Override
	protected void postProcessing()
	{
		if (config != null)
		{
			LdapSessionManager.releaseCurrentContext();
		}
	}

	protected InitialDirContext getLdapContext()
	{
		return LdapSessionManager.getLdapContext();
	}

	protected boolean isLdapConfigSpecified()
	{
		return config != null;
	}
}
