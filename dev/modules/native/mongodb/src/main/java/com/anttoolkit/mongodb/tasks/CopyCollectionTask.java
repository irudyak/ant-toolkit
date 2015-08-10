package com.anttoolkit.mongodb.tasks;

import java.util.*;

import com.mongodb.*;

import org.apache.tools.ant.*;

public class CopyCollectionTask extends GenericMongoTask
{
	private String src;
	private String dest;

	public void setSrc(String src)
	{
		this.src = src;
	}

	public void setDest(String dest)
	{
		this.dest = dest;
	}

	@Override
	public void doWork() throws BuildException
	{
		List<DBObject> ops = new ArrayList<DBObject>();
		ops.add(new BasicDBObject("$out", dest));

		try
		{
			DBCollection source = getMongoDatabase().getCollection(src);
			source.aggregate(ops);
		}
		catch (Throwable e)
		{
			throw new BuildException("Failed to copy Mongo collection '" + src + "' to '" + dest + "' collection", e);
		}
	}

	@Override
	protected void validate()
	{
		if (src == null || src.trim().isEmpty())
		{
			throw new BuildException("Source collection should be specified");
		}

		if (dest == null || dest.trim().isEmpty())
		{
			throw new BuildException("Dest collection should be specified");
		}

		if (!getMongoDatabase().collectionExists(src))
		{
			throw new BuildException("Source collection '" + src + "' doesn't exist");
		}

		if (getMongoDatabase().collectionExists(dest))
		{
			throw new BuildException("Dest collection '" + dest + "' already exist");
		}
	}
}
