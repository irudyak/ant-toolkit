package com.anttoolkit.mongodb.tasks;

import com.mongodb.gridfs.*;

import org.bson.types.*;

import org.apache.tools.ant.*;

public class DeleteFromGridFsTask extends GenericMongoTask
{
	private String bucket;
	private String objectId;
	private String query;

	public void setBucket(String bucket)
	{
		this.bucket = bucket;
	}

	public void setObjectId(String objectId)
	{
		this.objectId = objectId;
	}

	public void setQuery(String query)
	{
		this.query = query;
	}

	@Override
	public void doWork() throws BuildException
	{
		if (objectId != null && !objectId.trim().isEmpty())
		{
			deleteDocument(new ObjectId(objectId));
		}

		if (query != null && !query.trim().isEmpty())
		{
			deleteDocuments(query);
		}
	}

	@Override
	protected void validate()
	{
		if ((objectId == null || objectId.trim().isEmpty()) &&
			(query == null || query.trim().isEmpty()))
		{
			throw new BuildException("ObjectID and/or query to select documents to be deleted should be specified");
		}
	}

	private void deleteDocument(ObjectId objectId)
	{
		try
		{
			GridFS gridFs = bucket != null && !bucket.trim().isEmpty() ?
					new GridFS(getMongoDatabase(), bucket) :
					new GridFS(getMongoDatabase());

			gridFs.remove(objectId);
		}
		catch (Throwable e)
		{
			throw new BuildException("Failed to remove file from GridFs specified by id: " + objectId, e);
		}
	}

	private void deleteDocuments(String query)
	{
		try
		{
			GridFS gridFs = bucket != null && !bucket.trim().isEmpty() ?
					new GridFS(getMongoDatabase(), bucket) :
					new GridFS(getMongoDatabase());

			gridFs.remove(parseJson(query));
		}
		catch (Throwable e)
		{
			throw new BuildException("Failed to remove files from GridFs specified by query: " + query, e);
		}
	}
}
