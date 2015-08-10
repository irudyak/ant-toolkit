package com.anttoolkit.mongodb.tasks;

import java.util.*;

import com.mongodb.*;

import org.apache.tools.ant.*;

public class CheckIndexExistsTask extends GenericMongoTask
{
	private String collection;
	private String keys;
	private String name;
	private String property;

	public void setCollection(String collection)
	{
		this.collection = collection;
	}

	public void setProperty(String property)
	{
		this.property = property;
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
	public void doWork() throws BuildException
	{
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
			this.setPropertyThreadSafe(property, Boolean.FALSE.toString());
			return;
		}

		DBObject indexToLookFor = keys != null ? parseJson(keys) : null;

		for (DBObject index : indexes)
		{
			if (name != null && name.equals(index.get("name")))
			{
				this.setPropertyThreadSafe(property, Boolean.TRUE.toString());
				return;
			}

			if (indexToLookFor != null && indexToLookFor.equals(index.get("key")))
			{
				this.setPropertyThreadSafe(property, Boolean.TRUE.toString());
				return;
			}
		}

		this.setPropertyThreadSafe(property, Boolean.FALSE.toString());
	}

	@Override
	protected void validate()
	{
		if (collection == null || collection.trim().isEmpty())
		{
			throw new BuildException("Mongo collection for which to check index should be specified");
		}

		if ((keys == null || keys.trim().isEmpty()) &&
			(name == null || name.trim().isEmpty()))
		{
			throw new BuildException("Index keys or name should be specified");
		}

		if (property == null || property.trim().isEmpty())
		{
			throw new BuildException("Property to store check result should be specified");
		}
	}
}
