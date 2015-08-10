package com.anttoolkit.ldap.types;

import java.util.*;
import javax.naming.*;

import com.anttoolkit.general.common.*;

public class LdapConfig
{
	Hashtable<String, String> properties = new Hashtable<String, String>();

	public LdapConfig()
	{
		properties.put(Context.INITIAL_CONTEXT_FACTORY, "com.sun.jndi.ldap.LdapCtxFactory");
		properties.put(Context.REFERRAL, "follow");
	}

	public void setUrl(String url)
	{
		properties.put(Context.PROVIDER_URL, url);
	}

	public void setUser(String user)
	{
		properties.put(Context.SECURITY_AUTHENTICATION, "simple");
		properties.put(Context.SECURITY_PRINCIPAL, user);
	}

	public void setPassword(String password)
	{
		properties.put(Context.SECURITY_CREDENTIALS, password);
	}

	public void setHandleReferrals(boolean handle)
	{
		if (handle)
		{
			properties.put(Context.REFERRAL, "follow");
		}
		else
		{
			properties.put(Context.REFERRAL, "ignore");
		}
	}

	public void addConfiguredProperty(NameValueHolder holder)
	{
		properties.put(holder.getName(), holder.getValue());
	}

	public Hashtable<String, String> getProperties()
	{
		return properties;
	}
}
