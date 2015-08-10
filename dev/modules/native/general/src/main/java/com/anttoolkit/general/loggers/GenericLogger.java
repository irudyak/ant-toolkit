package com.anttoolkit.general.loggers;

import java.io.*;

import org.apache.tools.ant.*;
import org.apache.tools.ant.util.*;

public class GenericLogger extends DefaultLogger
{
	public static final String LOGS_DIR = "LOGS_DIR";

	private static final String TASK_INTEND = "  ";

	private long buildStartTime = System.currentTimeMillis();

	public void buildStarted(BuildEvent event)
	{
		buildStartTime = System.currentTimeMillis();
	}

	public void buildFinished(BuildEvent event)
	{
		Throwable error = event.getException();
		StringBuffer message = new StringBuffer();

		if (error == null)
		{
			message.append(StringUtils.LINE_SEP);
			message.append(getBuildSuccessfulMessage());
		}
		else
		{
			message.append(StringUtils.LINE_SEP);
			message.append(getBuildFailedMessage());
			message.append(StringUtils.LINE_SEP);
			message.append(StringUtils.getStackTrace(error));
		}

		message.append(StringUtils.LINE_SEP);
		message.append("Total time: ");
		message.append(formatTime(System.currentTimeMillis() - buildStartTime));

		String msg = message.toString();
		if (error == null)
		{
			printMessage(event.getProject(), msg, out, Project.MSG_VERBOSE);
		}
		else
		{
			printMessage(event.getProject(), msg, err, Project.MSG_ERR);
		}

		log(msg);
	}

	public void targetStarted(BuildEvent event)
	{
		if (event.getTarget().getName().trim().length() == 0)
		{
			return;
		}

		String msg = StringUtils.LINE_SEP + getMessagePrefix(event) + event.getTarget().getName() + ":";
		printMessage(event.getProject(), msg, out, event.getPriority());
		log(msg);
	}

	public void messageLogged(BuildEvent event)
	{
		int priority = event.getPriority();
		if (msgOutputLevel < priority && event.getException() == null)
		{
			return;
		}

		StringBuilder message = new StringBuilder();

		// Print out the name of the task if we're in one
		String name = event.getTask() != null ?
				event.getTask().getTaskName() : null;
		String label = name != null ?
				this.getMessagePrefix(event) + TASK_INTEND + "[" + name + "] " : this.getMessagePrefix(event) + " ";

        if (event.getTask() != null && !emacsMode)
		{
			BufferedReader reader = null;
			try
			{
				reader = new BufferedReader(new StringReader(event.getMessage()));
				String line = reader.readLine();
				boolean first = true;

				do
				{
					if (first)
					{
						if (line == null)
						{
							message.append(label);
							break;
						}
					}
					else
					{
						message.append(StringUtils.LINE_SEP);
					}

					first = false;
					message.append(label).append(line);
					line = reader.readLine();
				}
				while (line != null);
			}
			catch (IOException e)
			{
				// shouldn't be possible
				message.append(label).append(event.getMessage());
			}
			finally
			{
				if (reader != null)
				{
					FileUtils.close(reader);
				}
			}
        }
		else
		{
             //emacs mode or there is no task
             message.append(this.getMessagePrefix(event) + TASK_INTEND + event.getMessage());
        }

		Throwable ex = event.getException();
		if (ex != null)
		{
			message.append(StringUtils.LINE_SEP);
			message.append(processException(ex, label));
		}

		String msg = message.toString();
		if (priority != Project.MSG_ERR)
		{
			printMessage(event.getProject(), msg, out, priority);
		}
		else
		{
			printMessage(event.getProject(), msg, err, priority);
		}

		log(msg);
	}

	protected String getMessagePrefix(BuildEvent event)
	{
		return "";
	}

	protected String processException(Throwable e, String label)
	{
		StringBuilder message = new StringBuilder();
		BufferedReader reader = null;

		try
		{
			reader = new BufferedReader(new StringReader(StringUtils.getStackTrace(e)));
			String line = reader.readLine();
			boolean first = true;

			do
			{
				if (first)
				{
					if (line == null)
					{
						message.append(label);
						break;
					}
				}
				else
				{
					message.append(StringUtils.LINE_SEP);
				}

				first = false;
				message.append(label).append(line);
				line = reader.readLine();
			}
			while (line != null);
		}
		catch (IOException ex)
		{
			// shouldn't be possible
			message.append(label).append(e.toString());
		}
		finally
		{
			if (reader != null)
			{
				FileUtils.close(reader);
			}
		}

		return message.toString();
	}

	protected void printMessage(final Project project, final String message, final PrintStream stream, final int priority)
	{
    	printMessage(message, stream, priority);
 	}
}
