package com.anttoolkit.mongodb.tasks;

import com.mongodb.*;

import org.apache.tools.ant.*;

import com.anttoolkit.mongodb.common.*;

public class InsertTask extends GenericMongoTask
{
	private String text;
	private String collection;
	private WriteConcern writeConcern;

	public void setCollection(String collection)
	{
		this.collection = collection;
	}

	public void addText(String text)
	{
		this.text = text;
	}

	public void setFile(String file)
	{
		if (file == null || file.trim().isEmpty())
		{
			throw new BuildException("The name of the file containing JSON to insert into " +
					"Mongo collection can't be empty");
		}

		if (!this.fileExists(file))
		{
			throw new BuildException("Specified JSON file '" + file + "' doesn't exist");
		}

		text = this.loadFileContent(file);
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
			DBObject obj = parseJson(text);

			if (writeConcern != null)
			{
				getMongoDatabase().getCollection(collection).insert(obj, writeConcern);
			}
			else
			{
				getMongoDatabase().getCollection(collection).insert(obj);
			}
		}
		catch (Throwable e)
		{
			throw new BuildException("Failed to insert into '" + collection + "' collection given JSON: " + text, e);
		}
	}

	@Override
	protected void validate()
	{
		if (collection == null || collection.trim().isEmpty())
		{
			throw new BuildException("Mongo collection to insert JSON should be specified");
		}

		if (text == null || text.trim().isEmpty())
		{
			throw new BuildException("JSON to insert into Mongo collection should be specified");
		}
	}
}
