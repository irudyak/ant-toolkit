package com.anttoolkit.mongodb.tasks;

import org.apache.tools.ant.*;

import com.anttoolkit.mongodb.common.*;

public class CreateIndexTask extends GenericMongoTask
{
	private String collection;
	private String keys;
	private String options;

	public void setCollection(String collection)
	{
		this.collection = collection;
	}

	public void setKeys(String keys)
	{
		this.keys = keys;
	}

	public void setOptions(String options)
	{
		this.options = options;
	}

	public void addConfiguredKeys(ValueHolder holder)
	{
		this.keys = holder.getValue();
	}

	public void addConfiguredOptions(ValueHolder holder)
	{
		this.options = holder.getValue();
	}

	@Override
	public String toString()
	{
		return options == null || options.trim().isEmpty() ? keys : keys + ", " + options;
	}

	@Override
	public void doWork() throws BuildException
	{
		try
		{
			if (options != null && !options.trim().isEmpty())
			{
				getMongoDatabase().getCollection(collection).createIndex(parseJson(keys), parseJson(options));
			}
			else
			{
				getMongoDatabase().getCollection(collection).createIndex(parseJson(keys));
			}
		}
		catch (Throwable e)
		{
			throw new BuildException("Failed to create index '" + this.toString() + "' " +
					"for Mongo collection '" + collection + "'", e);
		}
	}

	@Override
	protected void validate()
	{
		if (collection == null || collection.trim().isEmpty())
		{
			throw new BuildException("Mongo collection for which to create index should be specified");
		}

		if (keys == null || keys.trim().isEmpty())
		{
			throw new BuildException("Index keys should be specified");
		}
	}
}
