package com.anttoolkit.documentum.tasks;

import org.apache.tools.ant.*;

import com.anttoolkit.documentum.common.*;

public class DocbaseCredentialsTask
		extends GenericDocbaseTask
{
	private String docbaseProperty = null;
	private String loginProperty = null;
	private String passwordProperty = null;
	private String domainProperty = null;

	public void setDocbaseProperty(String property)
	{
		docbaseProperty = property;
	}

	public void setLoginProperty(String property)
	{
		loginProperty = property;
	}

	public void setPasswordProperty(String property)
	{
		passwordProperty = property;
	}

	public void setDomainProperty(String property)
	{
		domainProperty = property;
	}

	public void doWork()
			throws BuildException
	{
		LoginInfo loginInfo = DocbaseSessionManager.getCurrentSessionContext();

		if (loginInfo == null)
		{
			throw new BuildException("Docbase session was not established");
		}

		if (docbaseProperty != null)
		{
			getProject().setProperty(docbaseProperty, loginInfo.getDocbase());
		}

		if (loginProperty != null)
		{
			getProject().setProperty(loginProperty, loginInfo.getLogin());
		}

		if (passwordProperty != null)
		{
			getProject().setProperty(passwordProperty, loginInfo.getPassword());
		}

		if (domainProperty != null)
		{
			getProject().setProperty(domainProperty, loginInfo.getDomain());
		}
	}
}
