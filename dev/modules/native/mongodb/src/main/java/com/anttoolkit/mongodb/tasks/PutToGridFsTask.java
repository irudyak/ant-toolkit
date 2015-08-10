package com.anttoolkit.mongodb.tasks;

import java.io.*;
import javax.activation.*;

import com.mongodb.gridfs.*;

import org.apache.tools.ant.*;

public class PutToGridFsTask extends GenericMongoTask
{
	private static final MimetypesFileTypeMap MIME_TYPES_MAP = new MimetypesFileTypeMap();

	private String file;
	private String bucket;
	private Long chunkSize;
	private String property;

	public void setFile(String file)
	{
		this.file = file;
	}

	public void setBucket(String bucket)
	{
		this.bucket = bucket;
	}

	public void setChunkSize(long chunkSize)
	{
		this.chunkSize = chunkSize;
	}

	public void setProperty(String property)
	{
		this.property = property;
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

			String fileAbsolutePath = fileObj.getAbsolutePath();
			int index = fileAbsolutePath.lastIndexOf(File.separatorChar);
			String shortName = index <= 0 ? fileAbsolutePath : fileAbsolutePath.substring(index + 1);

			String mimeType = null;

			try
			{
				mimeType = MIME_TYPES_MAP.getContentType(fileObj);
			}
			catch (Throwable e) {}

			GridFSInputFile gridFsFile = gridFs.createFile(fileObj);
			gridFsFile.setFilename(shortName);

			if (mimeType != null)
			{
				gridFsFile.setContentType(mimeType);
			}

			if (chunkSize != null)
			{
				gridFsFile.save(chunkSize);
			}
			else
			{
				gridFsFile.save();
			}

			if (property != null && !property.trim().isEmpty())
			{
				this.setPropertyThreadSafe(property, gridFsFile.getId().toString());
			}
		}
		catch (Throwable e)
		{
			throw new BuildException("Failed to save file '" + file + "' to GridFS", e);
		}
	}

	@Override
	protected void validate()
	{
		if (file == null || file.trim().isEmpty())
		{
			throw new BuildException("File to put to GridFS should be specified");
		}

		if (!this.fileExists(file))
		{
			throw new BuildException("Specified file '" + file + "' doesn't exist");
		}
	}
}
