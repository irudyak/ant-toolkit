package com.anttoolkit.maestro.types;

import org.apache.tools.ant.*;

import com.anttoolkit.general.tasks.*;
import com.anttoolkit.maestro.common.*;

public class MaestroConfig
{
	private String credentialsFile;
	private String userId;
	private String token;
	private String password;

	private boolean initialized = false;

	public void setCredentialsFile(String file)
	{
		this.credentialsFile = file;

	}

	public void setUserId(String userId)
	{
		this.userId = userId;
	}

	public void setToken(String token)
	{
		this.token = token;
	}

	public void setPassword(String password)
	{
		this.password = password;
	}

	public String getUserId(GenericTask task)
	{
		init(task);
		return userId;
	}

	public String getToken(GenericTask task)
	{
		init(task);
		return token;
	}

	private synchronized void init(GenericTask task)
	{
		if (initialized)
		{
			return;
		}

		if (credentialsFile != null && userId != null)
		{
			throw new BuildException("Either credentials file or userID and token/password should be specified");
		}

		if (credentialsFile == null && userId == null)
		{
			throw new BuildException("Credentials file or userID and token/password should be specified");
		}

		if (userId != null)
		{
			if (token != null && password != null)
			{
				throw new BuildException("Either token or password should be specified for userID: " + userId);
			}

			if (token == null && password == null)
			{
				throw new BuildException("Token or password should be specified for userID: " + userId);
			}
		}

		initCredentials(task);

		initialized = true;
	}

	private void initCredentials(GenericTask task)
	{
		if (userId != null)
		{
			if (token != null)
			{
				return;
			}

			token = MaestroHelper.obtainToken(userId, password);

			return;
		}

		String[] credentials = MaestroHelper.parseCredentialsFile(credentialsFile, task);

		if (credentials == null)
		{
			throw new BuildException("Incorrect credentials file specified: " + credentialsFile);
		}

		this.userId = credentials[0];
		this.token = credentials[1];
	}
}
