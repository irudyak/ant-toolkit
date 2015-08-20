package com.anttoolkit.general.tasks;

import java.lang.reflect.*;
import java.nio.charset.*;
import java.util.*;
import java.io.*;

import org.apache.tools.ant.*;

import com.anttoolkit.general.common.annotations.Required;
import com.anttoolkit.general.common.*;
import com.anttoolkit.general.refs.*;

public abstract class GenericTask
		extends Task
{
	private static final int READ_FILE_BUFFER_SIZE = 1024;

	private boolean failOnError = true;

	public static String substituteProperties(Project project,
											  String text)
			throws BuildException
	{
		if (text == null)
		{
			return null;
		}

		PropertyHelper helper = PropertyHelper.getPropertyHelper(project);
		return helper.replaceProperties(null, text, null);
	}

	public String substituteProperties(String text)
			throws BuildException
	{
		return substituteProperties(this.getProject(), text);
	}

	public static String getFileFullPath(Project project,
										 String fileName)
			throws BuildException
	{
		if(fileName == null || fileName.length() <= 0)
		{
			return fileName;
		}

		fileName = substituteProperties(project, fileName);

		File file = new File(fileName);
		if(file.isAbsolute())
		{
			return fileName;
		}

		file = new File(project.getBaseDir(), fileName);
		try
		{
			return file.getCanonicalPath();
		}
		catch(IOException e)
		{
			throw new BuildException("Failed to get file canonical path", e);
		}
	}

	public String getFileFullPath(String fileName)
			throws BuildException
	{
		return getFileFullPath(this.getProject(), fileName);
	}

	public void setFailonerror(String failOnError)
	{
		if (failOnError == null)
		{
			this.failOnError = true;
			return;
		}

		failOnError = failOnError.trim().toLowerCase();

		if (failOnError.equals("false") || failOnError.equals("no") ||
			failOnError.equals("off") || failOnError.equals("0"))
		{
			this.failOnError = false;
		}
		else if (failOnError.equals("true") || failOnError.equals("yes") ||
				failOnError.equals("on") || failOnError.equals("1"))
		{
			this.failOnError = true;
		}
		else
		{
			throw new BuildException("Incorrect value '" + failOnError + "' specified for failonerror attribute");
		}
	}

	public boolean failOnError()
	{
		return failOnError;
	}

	public void execute()
			throws BuildException
	{
		preValidate();
		validate();

		try
		{
			preProcessing();
			doWork();
		}
		catch (Throwable e)
		{
			if (!failOnError())
			{
				getProject().log(this, "Handled exception occured during [" + this.getTaskName() + "] task execution:", e, Project.MSG_ERR);
				return;
			}

			if (e instanceof BuildException)
			{
				throw (BuildException)e;
			}

			if (e instanceof RuntimeException)
			{
				throw (RuntimeException)e;
			}

			throw new BuildException("Exception occured:", e);
		}
		finally
		{
			postProcessing();
		}
	}

	public static void setPropertyThreadSafe(Project project, String name, String value)
	{
		if (project == null)
		{
			throw new NullPointerException("Project attribute can't be null");
		}

		if (name == null)
		{
			throw new NullPointerException("name attribute can't be null");
		}

		Object synch = SynchronizationManager.getSynchronizationObject("property", name);

		try
		{
			synchronized (synch)
			{
				project.setProperty(name, value);
			}
		}
		catch (ConcurrentModificationException e)	//paranoiac check
		{
			synchronized (System.class)
			{
				project.setProperty(name, value);
			}
		}
	}

	public final void setPropertyThreadSafe(String name, String value)
	{
		setPropertyThreadSafe(this.getProject(), name, value);
	}

	public final String loadFileContent(String fileName)
	{
		byte[] content = loadFileContentRaw(fileName);
		return content == null ? null : new String(content);
	}

	public final byte[] loadFileContentRaw(String fileName)
	{
		String fullPath = getFileFullPath(fileName);
		if (fullPath == null)
		{
			return null;
		}

		InputStream in = null;

		try
		{
			File file = new File(fullPath);
			int fileLength = (int)file.length();
			if (fileLength == 0)
			{
				return null;
			}

			in = new FileInputStream(file);

			byte[] buffer = new byte[fileLength];

			int offset = 0;
			int bytesRead = 0;

			while (offset < fileLength && bytesRead >= 0)
			{
				int bytesToRead = fileLength - offset < READ_FILE_BUFFER_SIZE ? fileLength - offset : READ_FILE_BUFFER_SIZE;
				bytesRead = in.read(buffer, offset, bytesToRead);

				offset += bytesRead;
			}

			return buffer;
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

	public final void saveContentToFile(String fileName, String content)
	{
		saveContentToFile(fileName, content.getBytes(Charset.forName("UTF-8")));
	}

	public final void saveContentToFile(String fileName, byte[] content)
	{
		String fullPath = getFileFullPath(fileName);

		File file = new File(fullPath);
		if (file.exists() && file.isDirectory())
		{
			throw new BuildException("Can't save content to file '" + fullPath + "' cause directory with the same name already exists");
		}

		if (file.exists())
		{
			try
			{
				if (!file.delete())
				{
					throw new BuildException("Failed to delete file '" + fullPath + "' before writing new content into it");
				}
			}
			catch (Throwable e)
			{
				throw new BuildException("Failed to delete file '" + fullPath + "' before writing new content into it", e);
			}
		}

		try
		{
			if (!file.createNewFile())
			{
				throw new BuildException("Failed to create new file '" + fullPath + "' to write content into it");
			}
		}
		catch (Throwable e)
		{
			throw new BuildException("Failed to create new file '" + fullPath + "' to write content into it", e);
		}

		FileOutputStream out = null;

		try
		{
			out = new FileOutputStream(file);
			out.write(content);
		}
		catch (Throwable e)
		{
			throw new BuildException("Failed to write content to file '" + fullPath + "'", e);
		}
		finally
		{
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

	public final boolean fileExists(String fileName)
	{
		File file = new File(getFileFullPath(fileName));
		return file.exists() && file.isFile();
	}

	public final boolean dirExists(String dirName)
	{
		File file = new File(getFileFullPath(dirName));
		return file.exists() && file.isDirectory();
	}

	public final Object getReference(String name)
	{
		return ReferenceManager.getReference(name, this.getProject());
	}

	public final void setReference(String name, Object obj)
	{
		ReferenceManager.setReference(name, obj, this.getProject());
	}

	public final void removeReference(String name)
	{
		ReferenceManager.removeReference(name, this.getProject());
	}

	public final boolean checkReferenceExists(String name)
	{
		return ReferenceManager.exists(name, this.getProject());
	}

	public abstract void doWork() throws BuildException;

	protected void preProcessing()
	{
	}

	protected void postProcessing()
	{
	}

	protected void validate()
	{
	}

	protected void initDelegateTask(Task delegateTask)
	{
		String name = delegateTask.getClass().getName();
		boolean standardTask = delegateTask.getClass().getPackage().getName().startsWith("org.apache.tools.ant");

		name = name.substring(name.lastIndexOf('.') + 1);
		name = standardTask ? name.toLowerCase() : name;

		this.initDelegateTask(delegateTask, name);
	}

	protected void initDelegateTask(Task delegateTask, String taskName)
	{
		if (delegateTask == null)
		{
			return;
		}

		delegateTask.setProject(this.getProject());
		delegateTask.init();
		delegateTask.setOwningTarget(this.getOwningTarget());
		delegateTask.setLocation(this.getLocation());
		delegateTask.setRuntimeConfigurableWrapper(this.getRuntimeConfigurableWrapper());
		delegateTask.setTaskName(taskName);
	}

	private void preValidate()
	{
		Field[] fields = ReflectionHelper.getDeclaredFields(this.getClass(), null);

		for (Field field: fields)
		{
			if (!field.isAnnotationPresent(Required.class))
			{
				continue;
			}

			Required required = field.getAnnotation(Required.class);
			Object value;

			try
			{
				value = field.get(this);
			}
			catch (IllegalAccessException e)
			{
				throw new BuildException("Failed to get value of the field '" + field.getName() + "'", e);
			}

			if (value == null || value.toString() == null || value.toString().trim().isEmpty())
			{
				throw new BuildException(required.value());
			}
		}
	}
}
