package com.anttoolkit.mongodb.tasks;

import java.io.*;

import com.mongodb.gridfs.*;

import org.bson.types.*;

import org.apache.tools.ant.*;

public class GetFromGridFsTask extends GenericMongoTask
{
	private String file;
	private String objectId;
	private String bucket;

	public void setFile(String file)
	{
		this.file = file;
	}

	public void setObjectId(String objectId)
	{
		this.objectId = objectId;
	}

	public void setBucket(String bucket)
	{
		this.bucket = bucket;
	}

	@Override
	public void doWork() throws BuildException
	{
		try
		{
			GridFS gridFs = bucket != null && !bucket.trim().isEmpty() ?
					new GridFS(getMongoDatabase(), bucket) :
					new GridFS(getMongoDatabase());

			File fileObj = new File(this.getFileFullPath(file));

			GridFSDBFile gridFsFile = gridFs.findOne(new ObjectId(objectId));
			if (gridFsFile == null)
			{
				throw new BuildException("There are no file in GridFS having id '" + objectId + "'");
			}

			gridFsFile.writeTo(fileObj);
		}
		catch (Throwable e)
		{
			throw new BuildException("Failed to get GridFS object '" + objectId + "' to local file '" + file + "'", e);
		}
	}

	@Override
	public void validate()
	{
		if (file == null || file.trim().isEmpty())
		{
			throw new BuildException("File name should be specified");
		}

		if (this.fileExists(file))
		{
			throw new BuildException("Specified file '" + file + "' already exists");
		}

		if (objectId == null || objectId.trim().isEmpty())
		{
			throw new BuildException("ObjectId should be specified");
		}
	}
}
