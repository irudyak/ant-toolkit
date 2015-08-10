package com.anttoolkit.documentum.tasks.api;

import com.anttoolkit.documentum.common.*;

import org.apache.tools.ant.*;

import java.io.*;

public class ApiExecTask
		extends GenericDocbaseTask
{
	private enum CommandType
	{
		EXEC,
		GET,
		SET
	}

	private String apiStatement = null;
	private String file = null;
	private String encoding = "UTF-8";
	private boolean echo = true;
	private String property = null;

	public void addText(String text)
	{
		apiStatement = getProject().replaceProperties(text);
	}

	public void setProperty(String property)
	{
		this.property = property;
	}

	public void setCommand(String command)
	{
		apiStatement = command;
	}

	public void setFile(String file)
	{
		this.file = file;
	}

	public void setEcho(boolean echo)
	{
		this.echo = echo;
	}

	public void setEncoding(String encoding)
	{
		this.encoding = encoding;
	}

	public void doWork()
			throws BuildException
	{
		if (apiStatement == null && file == null)
		{
			throw new BuildException("Either API statement or batch file should be specified");
		}

		if (apiStatement != null)
		{
			executeSingleStatement(apiStatement);
		}

		if (file != null)
		{
			executeStatementsFromFile();
		}
	}

	private void executeSingleStatement(String statement)
			throws BuildException
	{
		if (statement.trim().length() == 0)
		{
			return;
		}

		int index = statement.indexOf(",");
		if (index == -1)
		{
			throw new BuildException("Incorrect API statement");
		}

		if (echo)
		{
			log("Executing IAPI: " + statement);
		}

		//command
		String command = statement.substring(0, index);

		//args
		String args = statement.substring(index + 1);
		index = args.indexOf(",");
		args = index == - 1 ? "" : args.substring(index + 1);

		CommandType commandType = getCommandType(command);

		try
		{
			if (commandType.equals(CommandType.EXEC))
			{
				//noinspection deprecation
				this.getSession().getDfSession().apiExec(command, args);
			}
			else if (commandType.equals(CommandType.GET))
			{
				//noinspection deprecation
				String val = this.getSession().getDfSession().apiGet(command, args);
				if (property != null)
				{
					this.setPropertyThreadSafe(property, val);
				}
			}
			else if (commandType.equals(CommandType.SET))
			{
				String value = "";
				index = args.lastIndexOf(",");
				if (index != -1)
				{
					value = args.substring(index + 1);
					args = args.substring(0, index);
				}

				//noinspection deprecation
				this.getSession().getDfSession().apiSet(command, args, value);
			}
		}
		catch (Throwable e)
		{
			throw new BuildException("Failed to execute IAPI statement: " + statement, e);
		}
	}

	private void executeStatementsFromFile()
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

			String line;

			while ((line = reader.readLine()) != null)
			{
				executeSingleStatement(line);
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

	private CommandType getCommandType(String command)
	{
		if (GetCommands.COMMANDS.contains(command.trim().toLowerCase()))
		{
			return CommandType.GET;
		}

		if (SetCommands.COMMANDS.contains(command.trim().toLowerCase()))
		{
			return CommandType.SET;
		}

		return CommandType.EXEC;
	}
}
