package com.anttoolkit.documentum.tasks;

import java.util.*;

import org.apache.tools.ant.*;

import com.anttoolkit.documentum.common.*;
import com.anttoolkit.documentum.tasks.build.util.*;

public class DocbaseTaskContainer
		extends GenericDocbaseTask
		implements TaskContainer
{
	private String login = null;
	private String domain = null;
	private String password = null;
	private String docbase = null;

	private LoginInfo loginInfo = null;

	private List<Task> tasks = new LinkedList<Task>();

	public void setLogin(String login)
	{
		this.login = login;
	}

	public void setDomain(String domain)
	{
		this.domain = domain;
	}

	public void setPassword(String password)
	{
		this.password = password;
	}

	public void setDocbase(String docbase)
	{
		this.docbase = docbase;
	}

	public void addTask(Task task)
	{
		if (task == null)
		{
			return;
		}

		tasks.add(task);
	}

	public void doWork()
			throws BuildException
	{
		DocbaseSessionManager.setCurrentSessionContext(getLoginInfo());

		try
		{
			for (Task task : tasks)
			{
				task.perform();
			}
		}
		catch (Throwable e)
		{
			if (DocbaseSessionManager.getSession().isTransactionActive())
			{
				DocbaseSessionManager.getSession().abortTransaction();
			}

			if (e instanceof BuildException)
			{
				throw (BuildException)e;
			}

			if (e instanceof RuntimeException)
			{
				throw (RuntimeException)e;
			}

			throw new BuildException("Exception occured", e);
		}
		finally
		{
			DocbaseSessionManager.releaseCurrentSessionContext();
		}
	}

	protected void setLoginInfo(LoginInfo loginInfo)
	{
		this.loginInfo = loginInfo;
		login = loginInfo.getLogin();
		domain = loginInfo.getDomain();
		password = loginInfo.getPassword();
		docbase = loginInfo.getDocbase();
	}

	protected LoginInfo getLoginInfo()
	{
		if (loginInfo != null)
		{
			return loginInfo;
		}

		return loginInfo = new LoginInfo(login, domain, password, docbase);
	}
}
