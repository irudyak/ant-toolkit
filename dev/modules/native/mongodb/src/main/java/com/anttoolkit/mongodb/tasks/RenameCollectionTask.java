package com.anttoolkit.mongodb.tasks;

import org.apache.tools.ant.*;

public class RenameCollectionTask extends GenericMongoTask
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
		try
		{
			getMongoDatabase().getCollection(src).rename(dest);
		}
		catch (Throwable e)
		{
			throw new BuildException("Failed to rename Mongo collection from '" + src + "' to '" + dest + "'", e);
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
