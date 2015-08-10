package com.anttoolkit.mongodb.tasks;

import com.mongodb.*;

import org.apache.tools.ant.*;

import com.anttoolkit.mongodb.common.*;

public class RemoveTask extends GenericMongoTask
{
	private String collection;
	private String query;
	private WriteConcern writeConcern;

	public void setCollection(String collection)
	{
		this.collection = collection;
	}

	public void addText(String query)
	{
		this.query = query;
	}

	public void setWriteConcern(String writeConcern)
	{
		this.writeConcern = WriteConcernParser.parse(writeConcern);
	}

	@Override
	public void doWork() throws BuildException
	{
		try
		{
			if (writeConcern != null)
			{
				getMongoDatabase().getCollection(collection).remove(parseJson(query), writeConcern);
			}
			else
			{
				getMongoDatabase().getCollection(collection).remove(parseJson(query));
			}
		}
		catch (Throwable e)
		{
			throw new BuildException("Failed to remove documents from Mongo collection " +
					"'" + collection + "' using query: " + query, e);
		}
	}

	@Override
	protected void validate()
	{
		if (collection == null || collection.trim().isEmpty())
		{
			throw new BuildException("Mongo collection from which to remove items should be specified");
		}

		if (query == null || query.trim().isEmpty())
		{
			throw new BuildException("Query to remove items from Mongo collection should be specified");
		}
	}
}
