package com.anttoolkit.mongodb.tasks;

import org.apache.tools.ant.*;

public class DropCollectionTask extends GenericMongoTask
{
	private String collection;

	public void setCollection(String collection)
	{
		this.collection = collection;
	}

	@Override
	public void doWork() throws BuildException
	{
		try
		{
			if (!getMongoDatabase().collectionExists(collection))
			{
				return;
			}

			getMongoDatabase().getCollection(collection).drop();
		}
		catch (Throwable e)
		{
			throw new BuildException("Failed to drop Mongo collection: " + collection, e);
		}
	}

	@Override
	protected void validate()
	{
		if (collection == null || collection.trim().isEmpty())
		{
			throw new BuildException("Collection name should be specified");
		}
	}
}
