package com.anttoolkit.system.tools.packaging;

import java.io.*;

public class ResourceHelper
{
	private static final int BUFFER_LENGTH = 1024;

	public static void main(String[] args)
	{
		if (args == null || args.length < 2)
		{
			throw new RuntimeException("Resource name and file name should be specified");
		}

		copyJavaResourceToFile(args[0], args[1]);
	}

	private static void copyJavaResourceToFile(String resource, String file)
	{
		InputStream in = null;
		OutputStream out = null;

		try
		{
			in = getResourceInputStream(resource);
			out = getFileOutputStream(file);

			byte[] buffer = new byte[BUFFER_LENGTH];
			int bytesRead = 0;

			while (bytesRead >= 0)
			{
				bytesRead = in.read(buffer, 0, BUFFER_LENGTH);
				if (bytesRead > 0)
				{
					out.write(buffer, 0, bytesRead);
				}
			}
		}
		catch (IOException e)
		{
			throw new RuntimeException("Failed to load resource: " + resource, e);
		}
		finally
		{
			if (in != null)
			{
				try
				{
					in.close();
				}
				catch (IOException e) { }
			}

			if (out != null)
			{
				try
				{
					out.close();
				}
				catch (IOException e) {}
			}
		}
	}

	private static InputStream getResourceInputStream(String resource)
	{
		ClassLoader cL = ResourceHelper.class.getClassLoader();

		InputStream in = cL != null ?
				cL.getResourceAsStream(resource) :
				ClassLoader.getSystemResourceAsStream(resource);


		if (in == null)
		{
			throw new RuntimeException("Unable to find resource: " + resource);
		}

		return in;
	}

	private static OutputStream getFileOutputStream(String filePath)
	{
		File file = new File(filePath);
		if (file.exists() && file.isDirectory())
		{
			throw new RuntimeException("Can't save content to file '" + filePath + "' cause directory with the same name already exists");
		}

		if (file.exists())
		{
			try
			{
				if (!file.delete())
				{
					throw new RuntimeException("Failed to delete file '" + filePath + "' before writing new content into it");
				}
			}
			catch (Throwable e)
			{
				throw new RuntimeException("Failed to delete file '" + filePath + "' before writing new content into it", e);
			}
		}

		try
		{
			if (!file.createNewFile())
			{
				throw new RuntimeException("Failed to create new file '" + filePath + "' to write content into it");
			}
		}
		catch (Throwable e)
		{
			throw new RuntimeException("Failed to create new file '" + filePath + "' to write content into it", e);
		}

		try
		{
			return new FileOutputStream(file);
		}
		catch (IOException e)
		{
			throw new RuntimeException("Failed to create output stream for file:" + filePath, e);
		}
	}
}
