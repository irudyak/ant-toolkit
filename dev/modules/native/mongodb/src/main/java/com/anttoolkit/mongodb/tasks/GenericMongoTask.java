package com.anttoolkit.mongodb.tasks;

import com.mongodb.*;

import com.mongodb.util.JSON;
import com.mongodb.util.JSONCallback;
import org.apache.tools.ant.*;

import com.anttoolkit.general.tasks.*;
import com.anttoolkit.mongodb.types.*;
import com.anttoolkit.mongodb.common.*;

public abstract class GenericMongoTask extends GenericTask
{
	private MongoConfig config;

	public void setMongoConfig(String ref)
	{
		Object obj = getReference(ref);
		if (!(obj instanceof MongoConfig))
		{
			throw new IllegalArgumentException("Incorrect Mongo config specified");
		}

		this.config = (MongoConfig)obj;

		if (config.getServers().isEmpty())
		{
			throw new BuildException("Invalid Mongo config '" + config + "', no servers specified.");
		}

		if (config.getDatabase() == null || config.getDatabase().trim().isEmpty())
		{
			throw new BuildException("Invalid Mongo config '" + config + "', no database specified.");
		}

		if ((config.getUsername() == null && config.getPassword() != null) ||
			(config.getUsername() != null && config.getPassword() == null))
		{
			throw new BuildException("Invalid Mongo config '" + config + "', " +
					"either username and password should be specified or none of them should be specified.");
		}
	}

	@Override
	protected void preProcessing()
	{
		if (config != null)
		{
			MongoSessionManager.setCurrentContext(config);
		}
	}

	@Override
	protected void postProcessing()
	{
		if (config != null)
		{
			MongoSessionManager.releaseCurrentContext();
		}
	}

	protected DB getMongoDatabase()
	{
		return MongoSessionManager.getDatabase();
	}

	protected boolean isMongoConfigSpecified()
	{
		return config != null;
	}

	protected DBObject parseJson(String text)
	{
		if (text == null || text.trim().isEmpty())
		{
			return (DBObject)JSON.parse(null);
		}

		return (DBObject)JSON.parse(text, new EnhancedJsonCallback(this));
	}
}
