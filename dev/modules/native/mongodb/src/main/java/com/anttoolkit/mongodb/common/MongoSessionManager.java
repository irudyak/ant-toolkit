package com.anttoolkit.mongodb.common;

import java.net.*;
import java.util.*;

import com.mongodb.*;

import org.apache.tools.ant.*;

import com.anttoolkit.mongodb.types.*;

public class MongoSessionManager
{
	private static class MongoClientRef
	{
		private MongoClient client;
		private int refsCount = 0;

		public MongoClientRef(MongoClient client)
		{
			this.client = client;
			refsCount = 1;
		}

		public MongoClient getClient()
		{
			return client;
		}

		public void incrementRefs()
		{
			refsCount++;
		}

		public void decrementRefs()
		{
			if (refsCount == 0)
			{
				return;
			}

			refsCount--;

			if (refsCount == 0 && client != null)
			{
				client.close();
				client = null;
			}
		}

		public boolean isEmpty()
		{
			return client == null;
		}
	}

	private static final int MONGODB_DEFAULT_PORT = 27017;

	private static Map<String, MongoClientRef> clients = new HashMap<String, MongoClientRef>();

	private static ThreadLocal<Stack<MongoConfig>> configsStack = new ThreadLocal<Stack<MongoConfig>>()
	{
		protected Stack<MongoConfig> initialValue()
		{
			return new Stack<MongoConfig>();
		}
	};

	public static void setCurrentContext(MongoConfig config)
	{
		configsStack.get().push(config);
	}

	public static DB getDatabase()
	{
		try
		{
			MongoConfig config = configsStack.get().peek();
			MongoClientRef clientRef;

			synchronized (clients)
			{
				clientRef = clients.get(config.toString());
				if (clientRef == null)
				{
					clientRef = new MongoClientRef(createClient(config));
				}
				else
				{
					clientRef.incrementRefs();
				}
			}

			return clientRef.getClient().getDB(config.getDatabase());
		}
		catch (EmptyStackException e)
		{
			throw new BuildException("There are no MongoDB instances specified to connect to");
		}
	}

	public static void releaseCurrentContext()
	{
		try
		{
			MongoConfig config = configsStack.get().pop();

			synchronized (clients)
			{
				MongoClientRef clientRef = clients.get(config.toString());
				if (clientRef == null)
				{
					return;
				}

				clientRef.decrementRefs();
				if (clientRef.isEmpty())
				{
					clients.remove(clientRef.toString());
				}
			}
		}
		catch (EmptyStackException e) {}
	}

	private static MongoClient createClient(MongoConfig config)
	{
		List<ServerAddress> servers = new LinkedList<ServerAddress>();
		for (String server : config.getServers())
		{
			String[] host_port = server.split(":", -1);
			String host = host_port[0].trim();

			try
			{
				int port = host_port.length == 2 ? Integer.parseInt(host_port[1]) : MONGODB_DEFAULT_PORT;
				servers.add(new ServerAddress(host, port));
			}
			catch (NumberFormatException e)
			{
				throw new BuildException("Incorrect server port specification '" + server + "' in Mongo config");
			}
			catch (UnknownHostException e)
			{
				throw new BuildException("Incorrect server host specification '" + server + "' in Mongo config", e);
			}
		}

		List<MongoCredential> credentials = null;

		if (config.getUsername() != null)
		{
			credentials = new LinkedList<MongoCredential>();
			credentials.add(MongoCredential.createMongoCRCredential(config.getUsername(), config.getDatabase(), config.getPassword().toCharArray()));
		}

		MongoClientOptions.Builder builder = MongoClientOptions.builder();

		if (config.getReadPreference() != null)
		{
			builder.readPreference(config.getReadPreference());
		}

		if (config.getWriteConcern() != null)
		{
			builder.writeConcern(config.getWriteConcern());
		}

		MongoClientOptions options = config.getReadPreference() != null ||
				config.getWriteConcern() != null ? builder.build() : null;

		if (credentials != null)
		{
			return options == null ?
					new MongoClient(servers, credentials) :
					new MongoClient(servers, credentials, options);
		}

		return options == null ?
				new MongoClient(servers) :
				new MongoClient(servers, options);
	}
}
