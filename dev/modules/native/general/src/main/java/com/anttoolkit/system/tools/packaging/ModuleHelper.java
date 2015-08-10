package com.anttoolkit.system.tools.packaging;

import org.apache.tools.ant.*;

import java.io.*;
import java.lang.reflect.Array;
import java.util.*;
import java.util.regex.*;

public class ModuleHelper
{
	private static final Pattern VERSION_PATTERN = Pattern.compile("^[0-9]+\\.([0-9]*\\.*)*.*");

	private static final String NAME_PROPERTY = "name";
	private static final String DESCRIPTION_PROPERTY = "description";
	private static final String VERSION_PROPERTY = "version";
	private static final String URL_PROPERTY = "url";
	private static final String LIBS_PROPERTY = "libs";
	private static final String DEPENDENCIES_PROPERTY = "dependencies";
	private static final String TASKDEF_PROPERTY = "taskdef";
	private static final String TASKDEF_JAR_PROPERTY = "taskdefJar";

	public static String getNormalizedDependencyName(String fileName)
	{
		String normalized = fileName.trim().toLowerCase().replace("\\", "/");

		int index = normalized.lastIndexOf("/");
		if (index > 0)
		{
			normalized = normalized.substring(index + 1);
		}

		if (!normalized.endsWith(".jar"))
		{
			return normalized;
		}

		normalized = normalized.substring(0, normalized.length() - 4);

		String[] chunks = normalized.split("-");

		if (chunks.length == 1)
		{
			return normalized;
		}

		normalized = chunks[0];

		for (int i = 1; i < chunks.length ; i++)
		{
			if (VERSION_PATTERN.matcher(chunks[i]).matches())
			{
				return normalized;
			}

			normalized += "-" + chunks[i];
		}

		return normalized;
	}

	public static String getModuleDependencies(String moduleDescriptor)
	{
		ModuleInfo moduleInfo = getModuleInfo(moduleDescriptor);

		String[] pathsInfo = getModulePathsInfo(moduleDescriptor);
		String moduleRoot = pathsInfo[0];
		String modulesRoot = pathsInfo[1];

		File dir = new File(modulesRoot);

		File[] files = dir.listFiles();
		if (files == null || files.length == 0)
		{
			return null;
		}

		List<String> dependencies = new LinkedList<String>();

		for (File file : files)
		{
			String path;

			try
			{
				path = file.getCanonicalPath().replace("\\", "/");

				if (!file.isDirectory() || path.equals(moduleRoot) ||
					!(new File(path + "/target/package/module.properties")).exists())
				{
					continue;
				}
			}
			catch (IOException e)
			{
				throw new BuildException("Failed to get canonical path for file: " + file, e);
			}

			ModuleInfo dependencyModuleInfo = getModuleInfo(path + "/target/package/module.properties");

			boolean contains = false;
			for (String lib : moduleInfo.libs)
			{
				if (lib.equals(dependencyModuleInfo.taskdefJar))
				{
					contains = true;
					break;
				}
			}

			if (contains && !dependencies.contains(dependencyModuleInfo.name))
			{
				dependencies.add(dependencyModuleInfo.name);
			}
		}

		if (dependencies.isEmpty())
		{
			return null;
		}

		StringBuilder builder = new StringBuilder();
		for (String dependency : dependencies)
		{
			if (builder.length() > 0)
			{
				builder.append(",");
			}

			builder.append(dependency);
		}

		return builder.toString();
	}

	public static Map<String, ModuleInfo> getModulesInsideDir(String rootDir, Task task)
	{
		if (rootDir == null || task == null)
		{
			throw new IllegalArgumentException("Modules root dir and Ant task should be specified");
		}

		File dir = new File(rootDir);

		if (!dir.exists())
		{
			throw new IllegalArgumentException("Specified modules root dir doesn't exist: " + rootDir);
		}

		if (!dir.isDirectory())
		{
			throw new IllegalArgumentException("Specified modules root dir actually represents a file");
		}

		String canonicalPath;
		try
		{
			canonicalPath = dir.getCanonicalPath().replace("\\", "/");
		}
		catch (IOException e)
		{
			throw new BuildException("Failed to get canonical path for module root dir: " + rootDir, e);
		}

		task.log("---> Analysing modules inside: " + canonicalPath);

		File[] files = dir.listFiles();
		if (files == null || files.length == 0)
		{
			task.log("---> [WARNING] No modules found inside directory: " + canonicalPath, Project.MSG_WARN);
			return new HashMap<String, ModuleInfo>();
		}

		Map<String, ModuleInfo> modules = new HashMap<String, ModuleInfo>();

		for (File file : files)
		{
			if (!file.isDirectory())
			{
				continue;
			}

			String modulePath;
			try
			{
				modulePath = file.getCanonicalPath().replace("\\", "/");
				if (!(new File(modulePath + "/target/package/module.properties").exists()))
				{
					task.log("------> [WARNING] Directory '" + modulePath + "' doesn't have 'target/package/module.properties' file, so it will not be treated as module configuration directory", Project.MSG_WARN);
					continue;
				}
			}
			catch (IOException e)
			{
				throw new BuildException("Failed to get canonical path for file: " + file, e);
			}

			ModuleInfo moduleInfo = getModuleInfo(modulePath + "/target/package/module.properties");
			modules.put(moduleInfo.name, moduleInfo);

			task.log("------> Found module '" + moduleInfo.name + "' inside directory: " + moduleInfo.baseDir);
		}

		if (modules.isEmpty())
		{
			task.log("---> [WARNING] No modules found inside directory: " + canonicalPath, Project.MSG_WARN);
		}

		return modules;
	}

	private static ModuleInfo getModuleInfo(String moduleDescriptor)
	{
		InputStream in = null;
		Properties prop = null;

		try
		{
			in = new FileInputStream(new File(moduleDescriptor));
			prop = new Properties();
			prop.load(in);
		}
		catch (IOException e)
		{
			throw new BuildException("Failed to load module descriptor file: " + moduleDescriptor, e);
		}
		finally
		{
			if (in != null)
			{
				try
				{
					in.close();
				}
				catch (IOException e) {}
			}
		}

		String name = prop.getProperty(NAME_PROPERTY);
		String description = prop.getProperty(DESCRIPTION_PROPERTY);
		String version = prop.getProperty(VERSION_PROPERTY);
		String url = prop.getProperty(URL_PROPERTY);
		String libs = prop.getProperty(LIBS_PROPERTY);
		String dependencies = prop.getProperty(DEPENDENCIES_PROPERTY);
		String taskdef = prop.getProperty(TASKDEF_PROPERTY);
		String taskdefJar = prop.getProperty(TASKDEF_JAR_PROPERTY);

		if (name == null)
		{
			throw new BuildException("Incorrect module descriptor file specified, cause it doesn't contain 'name' property: " + moduleDescriptor);
		}

		if (libs == null)
		{
			throw new BuildException("Incorrect module descriptor file specified, cause it doesn't contain 'libs' property: " + moduleDescriptor);
		}

		if (taskdefJar == null)
		{
			throw new BuildException("Incorrect module descriptor file specified, cause it doesn't contain 'taskdefJar' property: " + moduleDescriptor);
		}

		if (taskdef == null)
		{
			throw new BuildException("Incorrect module descriptor file specified, cause it doesn't contain 'taskdef' property: " + moduleDescriptor);
		}

		return new ModuleInfo(name, description, version, url, taskdef, taskdefJar, libs.split(","), dependencies == null ? null : dependencies.split(","), moduleDescriptor);
	}

	private static String[] getModulePathsInfo(String moduleDescriptor)
	{
		String path;

		try
		{
			path = (new File(moduleDescriptor)).getCanonicalPath();
		}
		catch (IOException e)
		{
			throw new BuildException("Failed to get canonic path for module descriptor: " + moduleDescriptor, e);
		}

		path = path.replace("\\", "/");

		int index = path.lastIndexOf("/");
		if (index == -1)
		{
			return null;
		}

		path = path.substring(0, index);

		index = path.lastIndexOf("/");
		if (index == -1)
		{
			return null;
		}

		path = path.substring(0, index);

		index = path.lastIndexOf("/");
		if (index == -1)
		{
			return null;
		}

		String moduleRootPath = path.substring(0, index);

		index = moduleRootPath.lastIndexOf("/");
		if (index == -1)
		{
			return null;
		}

		String modulesRoot = moduleRootPath.substring(0, index);

		return new String[] {moduleRootPath, modulesRoot};
	}
}
