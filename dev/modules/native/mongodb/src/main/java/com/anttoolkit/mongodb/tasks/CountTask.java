package com.anttoolkit.mongodb.tasks;

import com.mongodb.*;

import org.apache.tools.ant.*;

import com.anttoolkit.mongodb.common.*;

public class CountTask extends GenericMongoTask
{
	private String collection;
	private String query;
	private String property;
	private ReadPreference readPreference;

	public void setCollection(String collection)
	{
		this.collection = collection;
	}

	public void addText(String query)
	{
		this.query = query;
	}

	public void setProperty(String property)
	{
		this.property = property;
	}

	public void setReadPreference(String readPreference)
	{
		this.readPreference = ReadPreferenceParser.parse(readPreference);
	}

	@Override
	public void doWork() throws BuildException
	{
		try
		{
			DBObject queryObj = query == null || query.trim().isEmpty() ? new BasicDBObject() : parseJson(query);

			long count = readPreference != null ?
					getMongoDatabase().getCollection(collection).getCount(queryObj, null, readPreference) :
					getMongoDatabase().getCollection(collection).getCount(queryObj);

			this.setPropertyThreadSafe(property, Long.toString(count));
		}
		catch (Throwable e)
		{
			throw new BuildException("Failed to get documents count from collection '" + collection + "' using query: " + (query == null ? "null" : query), e);
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
			throw new BuildException("Property to store count should be specified");
		}
	}
}
