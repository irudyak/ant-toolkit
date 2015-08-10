package com.anttoolkit.mongodb.tasks;

import java.text.*;
import java.util.*;

import com.mongodb.*;

import org.apache.tools.ant.*;

import com.anttoolkit.mongodb.common.*;

public class FindLoopTask
		extends GenericMongoTask
		implements TaskContainer
{
	private static final String INFO_TEMPLATE = "{query: {0}, projection: {1}, " +
			"limit: {2}, skip: {3}, sort: {4}, hint: {5}}";

	private List<Task> tasks = new LinkedList<Task>();

	private String collection;
	private String query;
	private String projection;
	private Integer limit;
	private Integer skip;
	private String sort;
	private String hint;

	private String reference;
	private String property;
	private ReadPreference readPreference;

	public void setCollection(String collection)
	{
		this.collection = collection;
	}

	public void setQuery(String query)
	{
		this.query = query;
	}

	public void setProjection(String projection)
	{
		this.projection = projection;
	}

	public void setLimit(int limit)
	{
		this.limit = limit;
	}

	public void setSkip(int skip)
	{
		this.skip = skip;
	}

	public void setSort(String sort)
	{
		this.sort = sort;
	}

	public void setHint(String hint)
	{
		this.hint = hint;
	}

	public void setReference(String reference)
	{
		this.reference = reference;
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
	public String toString()
	{
		return MessageFormat.format(INFO_TEMPLATE,
				query == null ? "{}" : query,
				projection == null ? "{}" : projection,
				limit == null ? "{}" : limit,
				skip == null ? "{}" : skip,
				sort == null ? "{}" : sort,
				hint == null ? "{}" : hint);
	}

	@Override
	public void doWork() throws BuildException
	{
		DBCursor cursor;

		try
		{
			cursor = find();
		}
		catch (Throwable e)
		{
			throw new BuildException("Failed to execute find operation against '" + collection + "' collection: " + toString(), e);
		}

		try
		{
			for (DBObject obj : cursor)
			{
				if (property != null)
				{
					this.setPropertyThreadSafe(property, obj.toString());
				}

				if (reference != null)
				{
					this.setReference(reference, obj);
				}

				for (Task task : tasks)
				{
					task.perform();
				}
			}
		}
		catch (Throwable e)
		{
			throw new BuildException("Failed to iterate through results of find operation: " + toString(), e);
		}
		finally
		{
			if (cursor != null)
			{
				try
				{
					cursor.close();
				}
				catch (Throwable e) {}
			}
		}
	}

	@Override
	public void addTask(Task task)
	{
		tasks.add(task);
	}

	@Override
	protected void validate()
	{
		if (collection == null || collection.trim().isEmpty())
		{
			throw new BuildException("Mongo collection should be specified");
		}

		if ((reference == null || reference.trim().isEmpty()) &&
			(property == null || property.trim().isEmpty()))
		{
			throw new BuildException("Reference or/and property to hold JSON object should be specified");
		}

		if (tasks.isEmpty())
		{
			throw new BuildException("No tasks specified to execute inside find loop");
		}
	}

	private DBCursor find()
	{
		DBCursor cursor = new DBCursor(getMongoDatabase().getCollection(collection),
				query == null || query.trim().isEmpty() ? new BasicDBObject() : parseJson(query),
				projection == null || projection.trim().isEmpty() ? null : parseJson(projection),
				readPreference);

		if (sort != null && !sort.trim().isEmpty())
		{
			cursor.sort(parseJson(sort));
		}

		if (limit != null)
		{
			cursor.limit(limit);
		}

		if (skip != null)
		{
			cursor.skip(skip);
		}

		if (hint != null && !hint.trim().isEmpty())
		{
			cursor.hint(hint);
		}

		return cursor;
	}
}
