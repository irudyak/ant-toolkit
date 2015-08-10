package com.anttoolkit.sql.tasks;

import java.io.*;
import java.util.*;

import com.anttoolkit.general.tasks.GenericTask;
import org.apache.tools.ant.*;
import org.apache.tools.ant.types.*;
import org.apache.tools.ant.types.resources.*;
import org.apache.tools.ant.util.*;

import com.anttoolkit.sql.common.*;

public class SqlUpdateQueryTask
		extends GenericTask
{
	public static class DelimiterType extends EnumeratedAttribute
	{
		public static final String NORMAL = "normal";
		public static final String ROW = "row";

		public String[] getValues()
		{
			return new String[] {NORMAL, ROW};
		}
	}

	private String delimiterType = DelimiterType.NORMAL;
	private String delimiter = ";";
	private String sqlCommand = "";
	private File file = null;
	private Union resources;
	private String encoding = "UTF-8";
	private boolean keepFormat = false;
	private boolean ignoreDelimiter = false;

	public void setSrc(File file)
	{
    	this.file = file;
	}

	public void addText(String sql)
	{
		sqlCommand += sql;
	}

	public void addFileset(FileSet set)
	{
		add(set);
	}

	public void add(ResourceCollection rc)
	{
		if (rc == null)
		{
			throw new BuildException("Cannot add null ResourceCollection");
		}

		synchronized (this)
		{
			if (resources == null)
			{
				resources = new Union();
			}
		}

		resources.add(rc);
	}

	public void setEncoding(String encoding)
	{
		this.encoding = encoding;
	}

	public void setDelimiter(String delimiter)
	{
		this.delimiter = delimiter;
	}

	public void setDelimiterType(DelimiterType delimiterType)
	{
		this.delimiterType = delimiterType.getValue();
	}

	public void setKeepFormat(boolean keepFormat)
	{
		this.keepFormat = keepFormat;
	}

	public void setIgnoreDelimiter(boolean ignore)
	{
		ignoreDelimiter = ignore;
	}

	public void doWork() throws BuildException
	{
		if (ignoreDelimiter)
		{
			delimiter = null;
			delimiterType = null;
		}

		if (sqlCommand.length() != 0)
		{
			Reader reader = new StringReader(sqlCommand);

			try
			{
				SqlHelper.executeUpdateStatements(SqlSessionManager.getSession(), reader,
						this.getProject(), keepFormat, delimiter, delimiterType, this);
			}
			finally
			{
				FileUtils.close(reader);
			}
		}

		if (file != null)
		{
			InputStream in = null;
			Reader reader = null;

			try
			{
				in = new FileResource(file).getInputStream();
				reader = encoding == null ? new InputStreamReader(in) : new InputStreamReader(in, encoding);

				SqlHelper.executeUpdateStatements(SqlSessionManager.getSession(), reader,
						this.getProject(), keepFormat, delimiter, delimiterType, this);
			}
			catch (IOException e)
			{
				throw new BuildException("Failed to read from file: " + file.getName(), e);
			}
			finally
			{
				FileUtils.close(in);
			    FileUtils.close(reader);
			}
		}

		if (resources != null)
		{
			InputStream in = null;
			Reader reader = null;

			Iterator iter = resources.iterator();
			while (iter.hasNext())
			{
				Resource resource = (Resource) iter.next();

				try
				{
					in = resource.getInputStream();
					reader = encoding == null ? new InputStreamReader(in) : new InputStreamReader(in, encoding);

					SqlHelper.executeUpdateStatements(SqlSessionManager.getSession(), reader,
							this.getProject(), keepFormat, delimiter, delimiterType, this);
				}
				catch (IOException e)
				{
					throw new BuildException("Failed to read from resource: " + resource.getName(), e);
				}
				finally
				{
					FileUtils.close(in);
					FileUtils.close(reader);
				}
			}
		}
	}

	protected void validate()
	{
		sqlCommand = sqlCommand.trim();

		if (file == null && sqlCommand.length() == 0 && resources == null)
		{
          throw new BuildException("File or resource collection or sql statement must be specified", getLocation());
		}

		if (file != null && !file.isFile())
		{
			throw new BuildException("Specified file " + file + " is not a file!", getLocation());
		}
	}
}
