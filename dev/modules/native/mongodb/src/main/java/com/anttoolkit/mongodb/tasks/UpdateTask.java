package com.anttoolkit.mongodb.tasks;

import com.mongodb.*;

import org.apache.tools.ant.*;

import com.fasterxml.jackson.databind.*;
import com.fasterxml.jackson.databind.node.*;
import com.fasterxml.jackson.core.*;

import com.anttoolkit.mongodb.common.*;

public class UpdateTask extends GenericMongoTask
{
	private String collection;
	private String query;
	private String update;
	private String options;
	private boolean multi = false;
	private boolean upsert = false;
	private WriteConcern writeConcern;

	@Override
	public String toString()
	{
		return "query: " + (query == null ? "null" : query) + ", " +
				"update: " + update + ", " +
				"options: " + (options == null ? "null" : options);
	}

	public void setCollection(String collection)
	{
		this.collection = collection;
	}

	public void setWriteConcern(String writeConcern)
	{
		this.writeConcern = WriteConcernParser.parse(writeConcern);
	}

	public void addConfiguredQuery(ValueHolder query)
	{
		this.query = query.getValue();
	}

	public void addConfiguredUpdate(ValueHolder update)
	{
		this.update = update.getValue();
	}

	public void addConfiguredOptions(ValueHolder options)
	{
		if (options == null || options.getValue() == null || options.getValue().trim().isEmpty())
		{
			this.options = null;
			multi = false;
			upsert = false;
			writeConcern = null;
			return;
		}

		this.options = options.getValue();

		ObjectMapper mapper = new ObjectMapper();
		mapper.configure(JsonParser.Feature.ALLOW_UNQUOTED_FIELD_NAMES, true);

		JsonNode node;

		try
		{
			node = mapper.readTree(options.getValue());
		}
		catch (Exception e)
		{
			throw new BuildException("Failed to parse update options JSON: " + options.getValue(), e);
		}

		if (!node.getNodeType().equals(JsonNodeType.OBJECT) || !node.isContainerNode())
		{
			throw new BuildException("Incorrect update options JSON: " + options.getValue());
		}

		JsonNode multiNode = node.get("multi");
		multi = multiNode != null && multiNode.booleanValue();

		JsonNode upsertNode = node.get("upsert");
		upsert = upsertNode != null && upsertNode.booleanValue();

		JsonNode writeConcerdNode = node.get("writeConcern");
		writeConcern = writeConcerdNode == null || writeConcerdNode.isNull() ? null : WriteConcernParser.parse(writeConcerdNode);
	}

	@Override
	public void doWork() throws BuildException
	{
		try
		{
			DBObject queryObj = query == null || query.trim().isEmpty() ? new BasicDBObject() : parseJson(query);
			DBObject updateObj = parseJson(update);

			if (writeConcern != null)
			{
				getMongoDatabase().getCollection(collection).update(queryObj, updateObj, upsert, multi, writeConcern);
			}
			else
			{
				getMongoDatabase().getCollection(collection).update(queryObj, updateObj, upsert, multi);
			}
		}
		catch (Throwable e)
		{
			throw new BuildException("Failed to update '" + collection + "' collection, " + toString(), e);
		}
	}

	@Override
	protected void validate()
	{
		if (collection == null || collection.trim().isEmpty())
		{
			throw new BuildException("Mongo collection should be specified");
		}

		if (update == null || update.trim().isEmpty())
		{
			throw new BuildException("Update expression for Mongo collection should be specified");
		}
	}
}
