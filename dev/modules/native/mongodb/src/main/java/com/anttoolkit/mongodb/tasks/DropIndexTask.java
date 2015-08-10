package com.anttoolkit.mongodb.tasks;

import java.util.*;

import com.mongodb.*;

import org.apache.tools.ant.*;

public class DropIndexTask extends GenericMongoTask
{
	private String collection;
	private String keys;
	private String name;

	public void setCollection(String collection)
	{
		this.collection = collection;
	}

	public void setKeys(String keys)
	{
		this.keys = keys;
	}

	public void setName(String name)
	{
		this.name = name;
	}

	@Override
	public String toString()
	{
		return name != null ? name : keys != null ? keys : "null";
	}

	@Override
	public void doWork() throws BuildException
	{
		if (name != null)
		{
			try
			{
				getMongoDatabase().getCollection(collection).dropIndex(name);
				return;
			}
			catch (Throwable e)
			{
				throw new BuildException("Failed to drop Mongo collection '" + collection +
						"' index '" + name + "'", e);
			}
		}

		List<DBObject> indexes;

		try
		{
			indexes = getMongoDatabase().getCollection(collection).getIndexInfo();
		}
		catch (Throwable e)
		{
			throw new BuildException("Failed to get list of indexes " +
					"for Mongo collection '" + collection + "'", e);
		}

		if (indexes == null || indexes.isEmpty())
		{
			throw new BuildException("Specified index '" + keys + "' doesn't exist " +
					"for collection '" + collection + "'");
		}

		DBObject indexToLookFor = keys != null ? parseJson(keys) : null;

		for (DBObject index : indexes)
		{
			Object obj = index.get("key");
			if (indexToLookFor != null && indexToLookFor.equals(obj))
			{
				try
				{
					getMongoDatabase().getCollection(collection).dropIndex(index.get("name").toString());
				}
				catch (Throwable e)
				{
					throw new BuildException("Failed to drop Mongo collection '" + collection +
							"' index '" + index.toString() + "'", e);
				}

				return;
			}
		}

		throw new BuildException("Specified index '" + this.toString() + "' doesn't exist " +
				"for collection '" + collection + "'");
	}

	@Override
	protected void validate()
	{
		if (collection == null || collection.trim().isEmpty())
		{
			throw new BuildException("Mongo collection for which to drop index should be specified");
		}

		if ((keys == null || keys.trim().isEmpty()) &&
			(name == null || name.trim().isEmpty()))
		{
			throw new BuildException("Index keys or name should be specified");
		}
	}
}
