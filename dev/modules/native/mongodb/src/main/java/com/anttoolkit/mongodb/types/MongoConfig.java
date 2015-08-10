package com.anttoolkit.mongodb.types;

import java.util.*;

import com.mongodb.*;

import org.apache.tools.ant.*;

import com.anttoolkit.mongodb.common.*;

public class MongoConfig
{
	private List<String> servers = new LinkedList<String>();
	private String database;
	private String username;
	private String password;
	private WriteConcern writeConcern;
	private ReadPreference readPreference;

	public void setServer(String server)
	{
		if (server == null || server.trim().isEmpty())
		{
			throw new BuildException("Server specification string can't be empty");
		}

		servers.add(server);
	}

	public void addConfiguredServer(ValueHolder server)
	{
		setServer(server.getValue());
	}

	public void setDatabase(String database)
	{
		if (database == null || database.trim().isEmpty())
		{
			throw new BuildException("MongoDB database name can't be empty");
		}

		this.database = database;
	}

	public void setUsername(String username)
	{
		if (username == null || username.trim().isEmpty())
		{
			throw new BuildException("MongoDB username can't be empty");
		}

		this.username = username;
	}

	public void setPassword(String password)
	{
		if (password == null || password.trim().isEmpty())
		{
			throw new BuildException("MongoDB password can't be empty");
		}

		this.password = password;
	}

	public void setWriteConcern(String writeConcern)
	{
		this.writeConcern = WriteConcernParser.parse(writeConcern);
	}

	public void addConfiguredWriteConcern(ValueHolder writeConcern)
	{
		this.writeConcern = WriteConcernParser.parse(writeConcern.getValue());
	}

	public void setReadPreference(String readPreference)
	{
		this.readPreference = ReadPreferenceParser.parse(readPreference);
	}

	public void addConfiguredReadPreference(ValueHolder readPreference)
	{
		this.readPreference = ReadPreferenceParser.parse(readPreference.getValue());
	}

	public List<String> getServers()
	{
		return Collections.unmodifiableList(servers);
	}

	public String getDatabase()
	{
		return database;
	}

	public String getUsername()
	{
		return username;
	}

	public String getPassword()
	{
		return password;
	}

	public WriteConcern getWriteConcern()
	{
		return writeConcern;
	}

	public ReadPreference getReadPreference()
	{
		return readPreference;
	}

	@Override
	public String toString()
	{
		StringBuilder builder = new StringBuilder();
		for (String server : servers)
		{
			builder.append(server);
		}

		builder.append(database);

		if (username != null)
		{
			builder.append(username);
		}

		if (password != null)
		{
			builder.append(password);
		}

		if (writeConcern != null)
		{
			builder.append(writeConcern.toString());
		}

		if (readPreference != null)
		{
			builder.append(readPreference.toString());
		}

		return builder.toString();
	}
}
