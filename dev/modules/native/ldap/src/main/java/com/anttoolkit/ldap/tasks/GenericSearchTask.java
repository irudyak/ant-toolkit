package com.anttoolkit.ldap.tasks;

import javax.naming.*;
import javax.naming.directory.*;

import org.apache.tools.ant.*;

import com.anttoolkit.ldap.types.*;

public abstract class GenericSearchTask
		extends GenericLdapTask
{
	private SearchConfig searchConfig;
	private String dn;
	private String filter;

	private NamingEnumeration<SearchResult> searchResult;

	public void setSearchConfig(String ref)
	{
		Object obj = this.getReference(ref);
		if (!(obj instanceof SearchConfig))
		{
			throw new IllegalArgumentException("Incorrect LDAP search configuration specified");
		}

		this.searchConfig = (SearchConfig)obj;
	}

	public void setSearchDn(String dn)
	{
		this.dn = dn;
	}

	public void setFilter(String filter)
	{
		this.filter = filter;
	}

	protected NamingEnumeration<SearchResult> search()
	{
		if (dn == null)
		{
			throw new BuildException("Base DN for LDAP search should be specified");
		}

		releaseResources();

		SearchControls searchCtrl = searchConfig != null ?
				searchConfig.getSearchControls() :
				SearchConfig.getDefaultSearchControls();

		try
		{
			return searchResult = getLdapContext().search(dn, filter, searchCtrl);
		}
		catch (NamingException e)
		{
			throw new BuildException("Failed to execute LDAP search", e);
		}
	}

	@Override
	protected void postProcessing()
	{
		releaseResources();
	}

	protected final boolean countLimitSpecified()
	{
		return searchConfig.countLimitSpecified();
	}

	private void releaseResources()
	{
		if (searchResult == null)
		{
			return;
		}

		try
		{
			searchResult.close();
			searchResult = null;
		}
		catch (NamingException e) {}
	}
}
