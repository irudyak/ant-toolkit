package com.anttoolkit.ldap.types;

import javax.naming.directory.*;
import java.util.*;

import com.anttoolkit.general.common.ValueHolder;
import org.apache.tools.ant.BuildException;

public class SearchConfig
{
	public static final String OBJECT_SCOPE = "OBJECT_SCOPE";
	public static String ONELEVEL_SCOPE = "ONELEVEL_SCOPE";
	public static String SUBTREE_SCOPE = "SUBTREE_SCOPE";

	private int scope = -1;
	private long countLimit = -1;
	private int timeLimit = -1;
	private Boolean handleReferrals = null;
	private ArrayList<String> attributes = null;

	public static SearchControls getDefaultSearchControls()
	{
		SearchControls ctrls = new SearchControls();
	    ctrls.setSearchScope(SearchControls.SUBTREE_SCOPE);
		ctrls.setDerefLinkFlag(true);
		return ctrls;
	}

	public void setScope(String scope)
	{
		if (scope == null || scope.trim().isEmpty())
		{
			throw new IllegalArgumentException("LDAP search scope couldn't be empty");
		}

		String _scope = scope.trim().toUpperCase().replace("SearchControls.", "");

		if (OBJECT_SCOPE.equals(_scope) || "0".equals(_scope))
		{
			this.scope = SearchControls.OBJECT_SCOPE;
		}
		else if (ONELEVEL_SCOPE.equals(_scope) || "1".equals(_scope))
		{
			this.scope = SearchControls.ONELEVEL_SCOPE;
		}
		else if (SUBTREE_SCOPE.equals(_scope) || "2".equals(_scope))
		{
			this.scope = SearchControls.SUBTREE_SCOPE;
		}

		if (this.scope == -1)
		{
			throw new IllegalArgumentException("Incorrect search scope specified: " + scope);
		}
	}

	public void setCountLimit(long limit)
	{
		if (limit < 0)
		{
			throw new IllegalArgumentException("Incorrect count limit specified: " + limit);
		}

		countLimit = limit;
	}

	public void setTimeLimit(int limit)
	{
		if (limit < 0)
		{
			throw new IllegalArgumentException("Incorrect time limit specified: " + limit);
		}

		timeLimit = limit;
	}

	public void setHandleReferrals(boolean handle)
	{
		handleReferrals = handle;
	}

	public void addConfiguredAttribute(ValueHolder holder)
	{
		if (attributes == null)
		{
			attributes = new ArrayList<String>();
		}

		attributes.add(holder.getValue());
	}

	public SearchControls getSearchControls()
	{
		validate();

		SearchControls ctrls = new SearchControls();

		ctrls.setSearchScope(scope);

		if (countLimit != -1)
		{
			ctrls.setCountLimit(countLimit);
		}

		if (timeLimit != -1)
		{
			ctrls.setTimeLimit(timeLimit);
		}

		if (handleReferrals != null && handleReferrals)
		{
			ctrls.setDerefLinkFlag(handleReferrals);
		}

		if (attributes != null && !attributes.isEmpty())
		{
			ctrls.setReturningAttributes(attributes.toArray(new String[]{}));
		}

		return ctrls;
	}

	public boolean countLimitSpecified()
	{
		return countLimit != -1;
	}

	private void validate()
	{
		if (scope == -1)
		{
			throw new BuildException("LDAP search config scope should be specified");
		}
	}
}
