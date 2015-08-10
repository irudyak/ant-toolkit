package com.anttoolkit.hadoop.tasks.hadoop.util;

import java.io.*;
import java.security.*;

import com.anttoolkit.hadoop.tasks.hadoop.GenericHadoopTask;
import org.apache.hadoop.conf.*;
import org.apache.hadoop.fs.*;
import org.apache.hadoop.security.*;
import org.apache.hadoop.security.UserGroupInformation.*;
import org.apache.tools.ant.*;

import com.anttoolkit.general.tasks.*;
import com.anttoolkit.hadoop.types.*;

public class AuthenticationContext
{
	private String user;
	private String principal;
	private String keytab;
	private AuthenticationMethod authMethod = AuthenticationMethod.SIMPLE;
	private boolean securityEstablished = false;

	public static AuthenticationContext instance(String user, String principal, String keytab)
	{
		return instance(user, principal, keytab, HadoopContextManager.getConfigContext());
	}

	public static AuthenticationContext instance(String user, String principal, String keytab, HadoopConfig config)
	{
		if ((principal != null && keytab != null) ||
			(config != null && config.getPrincipal() != null && config.getKeytab() != null))
		{
			String _principal = principal != null ? principal : config.getPrincipal();
			String _keytab = keytab != null ? keytab : config.getKeytab();
			return new AuthenticationContext(_principal, _keytab);
		}

		if (user != null || (config != null && config.getHadoopUser() != null))
		{
			String _user = user != null ? user : config.getHadoopUser();
			return new AuthenticationContext(_user);
		}

		return null;
	}

	public AuthenticationContext(String user)
	{
		if (user == null || user.trim().isEmpty())
		{
			throw new IllegalArgumentException("User name couldn't be empty");
		}

		this.user = user;
		authMethod = AuthenticationMethod.SIMPLE;
	}

	public AuthenticationContext(String principal, String keytab)
	{
		if (principal == null || principal.trim().isEmpty())
		{
			throw new IllegalArgumentException("Principal couldn't be empty");
		}

		if (keytab == null || keytab.trim().isEmpty())
		{
			throw new IllegalArgumentException("Keytab couldn't be empty");
		}

		this.principal = principal;
		this.keytab = keytab;
		authMethod = AuthenticationMethod.KERBEROS;
	}

	public <T> T executePrivilegedAction(Project project, Configuration conf, PrivilegedExceptionAction<T> action)
	{
		if (securityEstablished)
		{
			try
			{
				return action.run();
			}
			catch (Exception e)
			{
				throw new BuildException("Failed to execute Hadoop privileged action", e);
			}
		}

		if (!authMethod.equals(AuthenticationMethod.SIMPLE) &&
			!authMethod.equals(AuthenticationMethod.KERBEROS))
		{
			throw new BuildException("Authentication method '" + authMethod + "' is not supported");
		}

		try
		{

			UserGroupInformation ugi = authMethod.equals(AuthenticationMethod.SIMPLE) ?
					UserGroupInformation.getBestUGI(conf.get(CommonConfigurationKeys.KERBEROS_TICKET_CACHE_PATH), user) :
					UserGroupInformation.loginUserFromKeytabAndReturnUGI(principal, project == null ? keytab : GenericTask.getFileFullPath(project, keytab));

			securityEstablished = true;

			return ugi.doAs(action);
		}
		catch (IOException e)
		{
			throw new BuildException("Hadoop privileged action execution failed due to IO error", e);
		}
		catch (InterruptedException e)
		{
			throw new BuildException("Hadoop privileged action execution was interrupted", e);
		}
		finally
		{
			securityEstablished = false;
		}
	}

	public String getHadoopUser(Project project, Configuration conf)
	{
		if (!authMethod.equals(AuthenticationMethod.SIMPLE) &&
			!authMethod.equals(AuthenticationMethod.KERBEROS))
		{
			throw new BuildException("Authentication method '" + authMethod + "' is not supported");
		}

		try
		{
			UserGroupInformation ugi = authMethod.equals(AuthenticationMethod.SIMPLE) ?
					UserGroupInformation.getBestUGI(conf.get(CommonConfigurationKeys.KERBEROS_TICKET_CACHE_PATH), user) :
					UserGroupInformation.loginUserFromKeytabAndReturnUGI(principal, project == null ? keytab : GenericTask.getFileFullPath(project, keytab));

			return ugi.getShortUserName();
		}
		catch (IOException e)
		{
			throw new BuildException("Failed to get Hadoop short user name", e);
		}
	}

	public boolean equals(Object obj)
	{
		if (!(obj instanceof AuthenticationContext))
		{
			return false;
		}

		AuthenticationContext authContext = (AuthenticationContext)obj;

		if (!this.authMethod.equals(authContext.authMethod))
		{
			return false;
		}

		return this.authMethod.equals(AuthenticationMethod.SIMPLE) ?
				this.user.equals(authContext.user) :
				this.principal.equals(authContext.principal) && this.keytab.equals(authContext.keytab);
	}

	public int hashCode()
	{
		return this.authMethod.equals(AuthenticationMethod.SIMPLE) ?
				this.user.hashCode() :
				(this.principal + this.keytab).hashCode();
	}

	public String toString()
	{
		return this.authMethod.equals(AuthenticationMethod.SIMPLE) ?
				this.user :
				this.principal;
	}
}
