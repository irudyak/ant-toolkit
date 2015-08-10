package com.anttoolkit.hadoop.tasks.hdfs;

import java.io.*;
import java.util.zip.*;

import org.apache.hadoop.fs.*;
import org.apache.hadoop.io.*;
import org.apache.hadoop.io.compress.*;
import org.apache.tools.ant.*;

import com.anttoolkit.hadoop.tasks.hadoop.*;
import com.anttoolkit.hadoop.tasks.hdfs.util.*;

public class TextTask extends GenericHadoopTask
{
	private String remoteFile;
	private String localFile;

	public void setRemotefile(String file)
	{
		remoteFile = file;
	}

	public void setLocalFile(String file)
	{
		localFile = file;
	}

	@Override
	protected void doHadoopWork() throws BuildException
	{
		OutputStream out = null;
		InputStream in = null;

		try
		{
			in = getRemoteFileInput();
			out = getLocalFileOutput();

			try
			{
				IOUtils.copyBytes(in, out, getConfiguration(), false);
			}
			catch (IOException e)
			{
				throw new BuildException("Failed to transfer text from HDFS file \"" + remoteFile + "\" to local file \"" + localFile + "\"", e);
			}
		}
		finally
		{
			if (in != null)
			{
				try
				{
					in.close();
				}
				catch (Throwable e) {}
			}

			if (out != null)
			{
				try
				{
					out.close();
				}
				catch (Throwable e) {}
			}
		}

		getLocalFileOutput();
	}

	@Override
	protected void hadoopValidate()
	{
		if (remoteFile == null || remoteFile.trim().isEmpty())
		{
			throw new BuildException("Remote file should be specified");
		}

		if (localFile == null || localFile.trim().isEmpty())
		{
			throw new BuildException("Local file should be specified");
		}
	}

	private OutputStream getLocalFileOutput()
	{
		File file = new File(getFileFullPath(localFile));
		if (file.exists())
		{
			if (file.isDirectory())
			{
				throw new BuildException("Local directory having the same name as local file \"" + localFile + "\" is already exists");
			}

			try
			{
				if (!file.delete())
				{
					throw new BuildException("Failed to delete previously created local file: " + localFile);
				}
			}
			catch (Throwable e)
			{
				throw new BuildException("Failed to delete previously created local file: " + localFile);
			}
		}

		try
		{
			return new FileOutputStream(file, false);
		}
		catch (Throwable e)
		{
			throw new BuildException("Failed to create output stream for local file: " + localFile);
		}
	}

	private InputStream getRemoteFileInput()
	{
		try
		{
			FSDataInputStream in = getRemoteFileSystem().open(new Path(remoteFile));

			// Handle 0 and 1-byte files
			short leadBytes;
			try
			{
				leadBytes = in.readShort();
			}
			catch (EOFException e)
			{
				in.seek(0);
				return in;
			}

			// Check type of stream first
			switch(leadBytes)
			{
				case 0x1f8b:
				{ // RFC 1952
					// Must be gzip
					in.seek(0);
					return new GZIPInputStream(in);
				}
				case 0x5345:
				{ // 'S' 'E'
					// Might be a SequenceFile
					if (in.readByte() == 'Q')
					{
						in.close();
						return new TextRecordInputStream(getFileStatus(remoteFile), getConfiguration());
					}
				}
				default:
				{
					// Check the type of compression instead, depending on Codec class's
					// own detection methods, based on the provided path.
					CompressionCodecFactory cf = new CompressionCodecFactory(getConfiguration());
					CompressionCodec codec = cf.getCodec(new Path(remoteFile));
					if (codec != null)
					{
						in.seek(0);
						return codec.createInputStream(in);
					}

					break;
				}
				case 0x4f62:
				{ // 'O' 'b'
					if (in.readByte() == 'j')
					{
						in.close();
						return new AvroFileInputStream(getFileStatus(remoteFile));
					}

					break;
				}
			}

			// File is non-compressed, or not a file container we know.
			in.seek(0);
			return in;
		}
		catch (IOException e)
		{
			throw new BuildException("Failed to open remote file: " + remoteFile, e);
		}
	}
}
