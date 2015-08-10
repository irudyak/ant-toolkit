package com.anttoolkit.mongodb.tasks;

import com.mongodb.*;

import org.apache.tools.ant.*;

public class CreateCollectionTask
		extends GenericMongoTask
{
	private String collection;
	private Boolean isCapped;
	private Integer maxSize;
	private Integer maxCount;

	public void setCollection(String collection)
	{
		this.collection = collection;
	}

	public void setCapped(boolean isCapped)
	{
		this.isCapped = isCapped;
	}

	public void setMaxSize(int size)
	{
		this.maxSize = size;
	}

	public void setMaxCount(int count)
	{
		this.maxCount = count;
	}

	@Override
	public void doWork() throws BuildException
	{
		BasicDBObject dbObject = isCapped != null && isCapped ? new BasicDBObject("capped", true) : new BasicDBObject();

		if (maxSize != null)
		{
			dbObject.append("size", maxSize);

		}

		if (maxCount != null)
		{
			dbObject.append("max", maxCount);
		}

		try
		{
			getMongoDatabase().createCollection(collection, dbObject);
		}
		catch (Throwable e)
		{
			throw new BuildException("Failed to create '" + collection + "' collection", e);
		}
	}

	protected void validate()
	{
		if (collection == null || collection.trim().isEmpty())
		{
			throw new BuildException("Collectionname should be specified");
		}

		if (maxSize != null && isCapped == null)
		{
			throw new BuildException("maxSize parameter is only valid for capped collections");
		}

		if (maxCount != null && isCapped == null)
		{
			throw new BuildException("maxCount parameter is only valid for capped collections");
		}
	}
}
