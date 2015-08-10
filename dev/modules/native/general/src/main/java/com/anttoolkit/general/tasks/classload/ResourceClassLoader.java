package com.anttoolkit.general.tasks.classload;

import java.io.*;
import java.net.*;
import java.text.*;
import java.util.*;

import org.apache.tools.ant.*;
import org.apache.tools.ant.types.*;
import org.apache.tools.ant.types.resources.*;

public class ResourceClassLoader
		extends URLClassLoader
{
	private static final String JAR_FILE_URL_TEMPLATE = "jar:file:{0}!/";

	private Map<String, URL> fileResources;
	private List<URL> jarsAndDirs;
	private ClassLoader substitutedClassLoader;

	public ResourceClassLoader(Path path, List<File> extraResources)
	{
		super(getJarsAndDirs(path, extraResources).toArray(new URL[0]), Thread.currentThread().getContextClassLoader());
		this.fileResources = getFileResources(path, extraResources);
		this.jarsAndDirs = getJarsAndDirs(path, extraResources);
	}

	public URL findResource(String name)
	{
		URL resource = super.findResource(name);
		if (resource != null)
		{
			return resource;
		}

		if (fileResources == null || fileResources.isEmpty())
		{
			return null;
		}

		String _name = name.toLowerCase().replace("\\", "/");
		_name = _name.lastIndexOf("/") != - 1 ? _name.substring(_name.lastIndexOf("/") + 1).trim() : _name.trim();

		if (fileResources.containsKey(_name))
		{
			return fileResources.get(_name);
		}

		if (fileResources.containsKey(_name + ".properties"))
		{
			return fileResources.get(_name + ".properties");
		}

		return null;
	}

	public void substituteThreadClassLoader()
	{
		if ((fileResources == null || fileResources.isEmpty()) &&
			(jarsAndDirs == null || jarsAndDirs.isEmpty()))
		{
			return;
		}

		if (checkThreadAlreadyHasSameClassLoader())
		{
			return;
		}

		substitutedClassLoader = Thread.currentThread().getContextClassLoader();
		Thread.currentThread().setContextClassLoader(this);
	}

	public void restoreThreadClassLoader()
	{
		if (substitutedClassLoader != null)
		{
			Thread.currentThread().setContextClassLoader(substitutedClassLoader);
			substitutedClassLoader = null;
		}
	}

	private static boolean isJarFile(File file)
	{
		return file != null && file.isFile() &&
				file.getAbsolutePath().toLowerCase().trim().endsWith(".jar");
	}

	private static ArrayList<URL> getJarsAndDirs(Path path, List<File> extraResources)
	{
		ArrayList<URL> jarsAndDirs = new ArrayList<URL>();

		try
		{
			if (path != null)
			{
				for (Resource res : path)
				{
					if (!(res instanceof FileResource))
					{
						throw new IllegalArgumentException("Illegal resource specified for class loader: " + res.toString());
					}

					File file = ((FileResource)res).getFile();
					if (!file.exists())
					{
						throw new IllegalArgumentException("Specified resource doesn't exist: " + file.toString());
					}

					if (file.isDirectory())
					{
						if (!jarsAndDirs.contains(file.toURI().toURL()))
						{
							jarsAndDirs.add(file.toURI().toURL());
						}
					}
					else if (isJarFile(file))
					{
						URL url = new URL(MessageFormat.format(JAR_FILE_URL_TEMPLATE, file.getAbsolutePath()));

						if (!jarsAndDirs.contains(url))
						{
							jarsAndDirs.add(url);
						}
					}
				}
			}

			if (extraResources != null)
			{
				for (File file : extraResources)
				{
					if (!file.exists())
					{
						throw new IllegalArgumentException("Specified resource doesn't exist: " + file.toString());
					}

					if (file.isDirectory())
					{
						if (!jarsAndDirs.contains(file.toURI().toURL()))
						{
							jarsAndDirs.add(file.toURI().toURL());
						}
					}
					else if (isJarFile(file))
					{
						URL url = new URL(MessageFormat.format(JAR_FILE_URL_TEMPLATE, file.getAbsolutePath()));

						if (!jarsAndDirs.contains(url))
						{
							jarsAndDirs.add(url);
						}
					}
				}
			}
		}
		catch (MalformedURLException e)
		{
			throw new BuildException("Incorrect URL specified for class loader", e);
		}

		return jarsAndDirs;
	}

	private static Map<String, URL> getFileResources(Path path, List<File> extraResources)
	{
		Map<String, URL> fileRes = new HashMap<String, URL>();

		try
		{
			if (path != null)
			{
				for (Resource res : path)
				{
					if (!(res instanceof FileResource))
					{
						throw new IllegalArgumentException("Illegal resource specified for class loader: " + res.toString());
					}

					File file = ((FileResource)res).getFile();
					if (!file.exists())
					{
						throw new IllegalArgumentException("Specified resource doesn't exist: " + file.toString());
					}

					if (!file.isDirectory() && !isJarFile(file))
					{
						fileRes.put(file.getName(), file.toURI().toURL());
					}
				}
			}

			if (extraResources != null)
			{
				for (File file : extraResources)
				{
					if (!file.exists())
					{
						throw new IllegalArgumentException("Specified resource doesn't exist: " + file.toString());
					}

					if (!file.isDirectory() && !isJarFile(file))
					{
						fileRes.put(file.getName(), file.toURI().toURL());
					}
				}
			}
		}
		catch (MalformedURLException e)
		{
			throw new BuildException("Incorrect URL specified for class loader", e);
		}

		return fileRes;
	}

	private boolean checkThreadAlreadyHasSameClassLoader()
	{
		ClassLoader loader = Thread.currentThread().getContextClassLoader();

		while (loader != null)
		{
			if (loader instanceof ResourceClassLoader &&
				compareJarsAndDirs((ResourceClassLoader)loader) &&
				compareFileResources((ResourceClassLoader)loader))
			{
				return true;
			}

			loader = loader.getParent();
		}

		return false;
	}

	private boolean compareJarsAndDirs(ResourceClassLoader loader)
	{
		if (loader.jarsAndDirs.isEmpty() && jarsAndDirs.isEmpty())
		{
			return true;
		}

		if (loader.jarsAndDirs.size() != jarsAndDirs.size())
		{
			return false;
		}

		for (URL url : loader.jarsAndDirs)
		{
			if (!jarsAndDirs.contains(url))
			{
				return false;
			}
		}

		return true;
	}

	private boolean compareFileResources(ResourceClassLoader loader)
	{
		if (loader.fileResources.isEmpty() && fileResources.isEmpty())
		{
			return true;
		}

		if (loader.fileResources.size() != fileResources.size())
		{
			return false;
		}

		for (String name : loader.fileResources.keySet())
		{
			URL _url = loader.fileResources.get(name);
			URL url = fileResources.get(name);

			if ((_url == null && url != null) ||
				(_url != null && url == null) ||
				!_url.equals(url))
			{
				return false;
			}
		}

		return true;
	}
}
