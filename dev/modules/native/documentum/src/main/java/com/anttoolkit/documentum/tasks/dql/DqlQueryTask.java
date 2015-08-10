package com.anttoolkit.documentum.tasks.dql;

import com.anttoolkit.documentum.common.*;

import com.documentum.fc.common.*;

import org.apache.tools.ant.*;

import java.io.*;
import java.util.*;

public class DqlQueryTask
		extends GenericDocbaseTask
{
	private static final String IDQL_QUERY_DELIMITER = "go";

	private String dqlStatement = null;
	private String file = null;
	private String encoding = "UTF-8";
	private boolean echo = true;
	private String delimiter = IDQL_QUERY_DELIMITER;
	private String commentDelimiter = null;

	public void setFile(String file)
	{
		this.file = file;
	}

	public void setDelimiter(String delimiter)
	{
		this.delimiter = delimiter;

		if (this.delimiter == null || this.delimiter.trim().length() == 0)
		{
			throw new BuildException("Delimiter couldn't be null");
		}

		this.delimiter = this.delimiter.trim();
	}

	public void setCommentDelimiter(String delimiter)
	{
		commentDelimiter = delimiter;

		if (commentDelimiter != null)
		{
			commentDelimiter = commentDelimiter.trim().toLowerCase();
		}
	}

	public void setEncoding(String encoding)
	{
		this.encoding = encoding;
	}

	public void setEcho(boolean echo)
	{
		this.echo = echo;
	}

	public void setQuery(String query)
	{
		dqlStatement = query;
	}

	public void addText(String text)
	{
		dqlStatement = getProject().replaceProperties(text);
	}

	public void doWork()
			throws BuildException
	{
		if (dqlStatement == null && file == null)
		{
			throw new BuildException("DQL statement or file should be specified");
		}

		//execute single DQL query
		if (dqlStatement != null)
		{
			executeSingleQuery(dqlStatement);
		}

		//execute multiple DQL queries from xml file
		if (file != null)
		{
			executeQueriesFromFile();
		}
	}

	private void executeSingleQuery(String statement)
			throws BuildException
	{
		if (statement.trim().length() == 0)
		{
			return;
		}

		String query = statement.trim();

		if (echo)
		{
			log("Executing DQL query: " + query);
		}

		try
		{
			DqlHelper.executeQuery(this.getSession(), query);
		}
		catch (DfException e)
		{
			throw new BuildException("Failed to execute DQL query: " + query, e);
		}
	}

	private void executeQueriesFromFile()
			throws BuildException
	{
		String fullPath = getFileFullPath(file);
		if (fullPath == null)
		{
			throw new BuildException("File should be specified");
		}

		InputStream in = null;
		BufferedReader reader = null;

		try
		{
			File file = new File(fullPath);
			if (!file.isFile() || !file.exists())
			{
				throw new BuildException("Incorrect file specified: " + fullPath);
			}

			int fileLength = (int)file.length();
			if (fileLength == 0)
			{
				return;
			}

			in = new FileInputStream(file);
			reader = encoding == null ?
					new BufferedReader(new InputStreamReader(in)) :
					new BufferedReader(new InputStreamReader(in, encoding));

			StringBuffer buffer = new StringBuffer();
			String line;

			while ((line = reader.readLine()) != null)
			{
				line = getProject().replaceProperties(line);

				if (commentDelimiter != null &&
					commentDelimiter.trim().length() > 0 &&
					line.trim().toLowerCase().startsWith(commentDelimiter))
				{
					continue;
				}

				buffer.append(" ").append(line);

				if (delimiter != null)
				{
					int lastDelimPos = lastDelimiterPosition(buffer, delimiter);
					if (lastDelimPos > -1)
					{
						executeSingleQuery(buffer.substring(0, lastDelimPos));
						buffer.replace(0, buffer.length(), "");
					}
				}
    		}

			if (buffer.length() > 0)
			{
				executeSingleQuery(buffer.toString());
			}
		}
		catch (FileNotFoundException e)
		{
			throw new BuildException("Failed to open file " + fullPath, e);
		}
		catch (IOException e)
		{
			throw new BuildException("Failed read file " + fullPath, e);
		}
		finally
		{
			if (reader != null)
			{
				try
				{
					reader.close();
				}
				catch (Throwable ex) {}
			}

			if (in != null)
			{
				try
				{
					in.close();
				}
				catch (Throwable ex) {}
			}
		}
	}

	private static int lastDelimiterPosition(StringBuffer buf, String delimiter)
	{
		String _delimiter = delimiter.trim().toLowerCase(Locale.ENGLISH);

		int endIndex = delimiter.length() - 1;
		int bufferIndex = buf.length() - 1;

		while (bufferIndex >= 0 && Character.isWhitespace(buf.charAt(bufferIndex)))
		{
			--bufferIndex;
		}

		if (bufferIndex < endIndex)
		{
			return -1;
		}

		while (endIndex >= 0)
		{
			if (buf.substring(bufferIndex, bufferIndex + 1).toLowerCase(Locale.ENGLISH).charAt(0) != _delimiter.charAt(endIndex))
			{
				return -1;
			}

			bufferIndex--;
			endIndex--;
		}

		return bufferIndex + 1;
	}
}
