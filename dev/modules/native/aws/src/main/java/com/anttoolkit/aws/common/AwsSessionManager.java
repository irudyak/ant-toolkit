package com.anttoolkit.aws.common;

import java.util.*;

import com.amazonaws.*;
import com.amazonaws.regions.*;

import org.apache.tools.ant.*;

import com.anttoolkit.general.tasks.*;
import com.anttoolkit.aws.types.*;

public class AwsSessionManager
{
	private static ThreadLocal<Stack<AwsConfig>> configsStack = new ThreadLocal<Stack<AwsConfig>>()
	{
		protected Stack<AwsConfig> initialValue()
		{
			return new Stack<AwsConfig>();
		}
	};

	private static ThreadLocal<Stack<Map<Class<? extends AmazonWebServiceClient>, AmazonWebServiceClient>>> awsClients = new ThreadLocal<Stack<Map<Class<? extends AmazonWebServiceClient>, AmazonWebServiceClient>>>()
	{
		protected Stack<Map<Class<? extends AmazonWebServiceClient>, AmazonWebServiceClient>> initialValue()
		{
			return new Stack<Map<Class<? extends AmazonWebServiceClient>, AmazonWebServiceClient>>();
		}
	};

	public static void setCurrentContext(AwsConfig config)
	{
		configsStack.get().push(config);
		awsClients.get().push(new HashMap<Class<? extends AmazonWebServiceClient>, AmazonWebServiceClient>());
	}

	public static AmazonWebServiceClient getClient(Class<? extends AmazonWebServiceClient> clazz, GenericTask task)
	{
		Map<Class<? extends AmazonWebServiceClient>, AmazonWebServiceClient> clients;
		AwsConfig conf;

		try
		{
			conf = configsStack.get().peek();
			clients = awsClients.get().peek();
		}
		catch (EmptyStackException e)
		{
			throw new BuildException("There are no AWS configurations specified to connect");
		}


		if (clients.containsKey(clazz))
		{
			return clients.get(clazz);
		}

		AmazonWebServiceClient client;

		try
		{
			client = conf.getRegion().createClient(clazz, conf.getCredentialsProvider(task), conf.getClientConfiguration());
		}
		catch (Throwable e)
		{
			throw new BuildException("Failed to instantiate AWS client class " + clazz.getName(), e);
		}

		clients.put(clazz, client);

		return client;
	}

	public static Region getRegion()
	{
		try
		{
			AwsConfig conf = configsStack.get().peek();
			return conf.getRegion();
		}
		catch (EmptyStackException e)
		{
			throw new BuildException("There are no AWS configurations specified to connect");
		}
	}

	public static String getAvailabilityZone()
	{
		try
		{
			AwsConfig conf = configsStack.get().peek();
			return conf.getAvailabilityZone();
		}
		catch (EmptyStackException e)
		{
			throw new BuildException("There are no AWS configurations specified to connect");
		}
	}

	public static void releaseCurrentContext()
	{
		try
		{
			configsStack.get().pop();
		}
		catch (EmptyStackException e) {}

		Map<Class<? extends AmazonWebServiceClient>, AmazonWebServiceClient> clients = null;

		try
		{
			clients = awsClients.get().pop();
		}
		catch (EmptyStackException e) {}

		if (clients != null)
		{
			for (AmazonWebServiceClient client : clients.values())
			{
				try
				{
					client.shutdown();
				}
				catch (Throwable e) {}
			}
		}
	}
}
