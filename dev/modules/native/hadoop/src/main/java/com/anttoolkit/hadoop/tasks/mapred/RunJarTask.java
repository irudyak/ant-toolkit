package com.anttoolkit.hadoop.tasks.mapred;

import java.io.*;
import java.lang.reflect.*;
import java.util.*;
import java.util.jar.*;

import com.anttoolkit.general.common.JarHelper;
import com.anttoolkit.general.common.ValueHolder;
import org.apache.tools.ant.*;

import com.anttoolkit.hadoop.tasks.hadoop.*;

public class RunJarTask
		extends GenericHadoopTask
{
	private String jarFile;
	private String mainClassName;
	private ArrayList<String> args = new ArrayList<String>();

	public void setJar(String jarFile)
	{
		this.jarFile = getFileFullPath(jarFile);

		File file = new File(this.jarFile);
		if (!file.exists() || file.isDirectory() ||
			!file.getAbsolutePath().trim().toLowerCase().endsWith(".jar"))
		{
			throw new IllegalArgumentException("Incorrect jar file specified: " + jarFile);
		}
	}

	public void setClass(String clazz)
	{
		this.mainClassName = clazz == null ? null : clazz.replace("/", ".");
	}

	public void setArgs(String str)
	{
		if (str == null || str.trim().isEmpty())
		{
			return;
		}

		StringTokenizer tokenizer = new StringTokenizer(str, " ");

		while (tokenizer.hasMoreTokens())
		{
			args.add(tokenizer.nextToken());
		}
	}

	public void addConfiguredArg(ValueHolder arg)
	{
		args.add(arg.getValue());
	}

	@Override
	public void doHadoopWork() throws BuildException
	{
		try
		{
			String className = getJarMainClass();

			log("Executing main method of the " + className + " class from " + jarFile);

			Class<?> mainClass = Class.forName(className, true, Thread.currentThread().getContextClassLoader());
   			Method main = mainClass.getMethod("main", new Class[] {Array.newInstance(String.class, 0).getClass()});
			main.invoke(null, new Object[] {args.toArray(new String[0])});
		}
		catch (ClassNotFoundException e)
		{
			throw new BuildException("Failed to load java class '" + mainClassName + "' to execute main method", e);
		}
		catch (NoSuchMethodException e)
		{
			throw new BuildException("Java class '" + mainClassName + "' doesn't have main method", e);
		}
		catch (IllegalAccessException e)
		{
			throw new BuildException("Access exception occured while trying to execute main method of the class: " + mainClassName, e);
		}
		catch (InvocationTargetException e)
		{
			throw new BuildException("Exception occured during main method execution of the class: " + mainClassName, e);
		}
	}

	@Override
	protected List<File> provideExtraResourcesToHadoopClassLoader()
	{
		if (jarFile == null || jarFile.trim().isEmpty())
		{
			throw new BuildException("There is no jar file was specified for execution");
		}

		List<File> list = new LinkedList<File>();
		list.add(new File(jarFile));
		return list;
	}

	private String getJarMainClass()
	{
		if (mainClassName != null && !mainClassName.trim().isEmpty())
		{
			return mainClassName;
		}

		mainClassName = JarHelper.getMainClass(jarFile);

		if (mainClassName == null || mainClassName.trim().isEmpty())
		{
			throw new BuildException("Main class doesn't specified and jar manifest also doesn't contain info about class to execute");
		}

		return mainClassName;
  	}
}
