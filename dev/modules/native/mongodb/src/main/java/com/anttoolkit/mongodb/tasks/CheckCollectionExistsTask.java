package com.anttoolkit.mongodb.tasks;

import org.apache.tools.ant.*;

public class CheckCollectionExistsTask extends GenericMongoTask
{
	private String collection;
	private String property;

	public void setCollection(String collection)
	{
		this.collection = collection;
	}

	public void setProperty(String property)
	{
		this.property = property;
	}

	@Override
	public void doWork() throws BuildException
	{
		try
		{
			this.setPropertyThreadSafe(property, Boolean.toString(getMongoDatabase().collectionExists(collection)));
		}
		catch (Throwable e)
		{
			throw new BuildException("Failed to check if '" + collection + "' collection exists", e);
		}
	}

	@Override
	protected void validate()
	{
		if (collection == null || collection.trim().isEmpty())
		{
			throw new BuildException("Collection name should be specified");
		}

		if (property == null || property.trim().isEmpty())
		{
			throw new BuildException("Property name should be specified");
		}
	}
}
