package com.anttoolkit.ldap.common;

import java.util.*;
import javax.naming.*;
import javax.naming.directory.*;

import com.anttoolkit.ldap.types.*;
import org.apache.tools.ant.BuildException;

public class LdapSessionManager
{
	private static class LdapContextRef
	{
		private Hashtable<String, String> properties;
		private InitialDirContext context;

		public LdapContextRef(LdapConfig config)
		{
			properties = config.getProperties();
		}

		public InitialDirContext getContext()
		{
			if (context != null)
			{
				return context;
			}

			try
			{
				return context = new InitialDirContext(properties);
			}
			catch (NamingException e)
			{
				throw new BuildException("Failed to create LDAP context", e);
			}
		}

		public void releaseContext()
		{
			if (context == null)
			{
				return;
			}

			try
			{
				context.close();
			}
			catch (NamingException e)
			{
			}
			finally
			{
				context = null;
			}
		}
	}

	private static ThreadLocal<Stack<LdapContextRef>> ctxStack = new ThreadLocal<Stack<LdapContextRef>>()
	{
		protected Stack<LdapContextRef> initialValue()
		{
			return new Stack<LdapContextRef>();
		}
	};

	public static void setCurrentContext(LdapConfig config)
	{
		ctxStack.get().push(new LdapContextRef(config));
	}

	public static InitialDirContext getLdapContext()
	{
		try
		{
			LdapContextRef ctx = ctxStack.get().peek();
			return ctx.getContext();
		}
		catch (EmptyStackException e)
		{
			throw new BuildException("There are no LDAP instances specified to connect to");
		}
	}

	public static void releaseCurrentContext()
	{
		try
		{
			LdapContextRef ctx = ctxStack.get().pop();
			ctx.releaseContext();
		}
		catch (EmptyStackException e) {}
	}
}
