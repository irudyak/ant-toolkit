package com.anttoolkit.system.tools.packaging;

import java.io.*;
import java.util.*;

import org.apache.tools.ant.*;

import com.anttoolkit.general.tasks.collections.array.util.*;

public class FrameworkHelper
{
	private String frameworkBasedir;
	private List<String> modulesRootDirs = new LinkedList<String>();

	private Collection<ModuleInfo> modules = null;

	public FrameworkHelper(String frameworkBasedir, String modulesRootDirs)
	{
		if (frameworkBasedir == null || frameworkBasedir.trim().isEmpty())
		{
			throw new IllegalArgumentException("Framework basedir should be specified");
		}

		if (modulesRootDirs == null || modulesRootDirs.trim().isEmpty())
		{
			throw new IllegalArgumentException("Modules root dirs should be specified");
		}

		File dir = new File(frameworkBasedir);

		try
		{
			this.frameworkBasedir = dir.getCanonicalPath();
		}
		catch (IOException e)
		{
			throw new BuildException("Failed to get canonical path for directory: " + dir);
		}

		if (!dir.exists() || !dir.isDirectory())
		{
			throw new IllegalArgumentException("Incorrect framework basedir specified: " + this.frameworkBasedir);
		}

		String[] dirs = modulesRootDirs.split(",");

		for (String directory : dirs)
		{
			dir = new File(directory);
	        String path;

			try
			{
				path = dir.getCanonicalPath();
				this.modulesRootDirs.add(path);
			}
			catch (IOException e)
			{
				throw new BuildException("Failed to get canonical path for directory: " + directory);
			}

			if (!dir.exists() || !dir.isDirectory())
			{
				throw new IllegalArgumentException("Incorrect framework modules root dir specified: " + path);
			}
		}

		if (this.modulesRootDirs.isEmpty())
		{
			throw new BuildException("There are no valid modules root directories specified");
		}
	}

	public void initModulesTobePackaged(Task task, String namesArray, String pathsArray, String taskdefsArray)
	{
		if (modules == null)
		{
			modules = getListOfModulesToBePackaged(task);
		}

		List<String> names = new LinkedList<String>();
		List<String> paths = new LinkedList<String>();
		List<String> taskdefs = new LinkedList<String>();

		task.log("");
		task.log("Final set of modules to be packaged:");

		for (ModuleInfo moduleInfo : modules)
		{
			task.log("---> " + moduleInfo.name);
			names.add(moduleInfo.name);
			paths.add(moduleInfo.baseDir);
			taskdefs.add(moduleInfo.taskdef);
		}

		ArrayManager.init(namesArray, names, false);
		ArrayManager.init(pathsArray, paths, false);
		ArrayManager.init(taskdefsArray, taskdefs, false);
	}

	public void createModulesDescriptor(String file)
	{
		if (modules == null)
		{
			throw new BuildException("You should first initialized modules to be packaged");
		}

		OutputStream out;
		PrintStream stream;

		try
		{
			out = new FileOutputStream(file);
			stream = new PrintStream(out, true, "UTF-8");
		}
		catch (Throwable e)
		{
			throw new BuildException("Failed to create modules descriptor: " + file, e);
		}

		try
		{
			stream.println("<?xml version=\"1.0\"?>");
			stream.println("<modules>");

			for (ModuleInfo moduleInfo : modules)
			{
				stream.println("	<module name='" + moduleInfo.name + "'>");
				stream.println("		<version>" + moduleInfo.version + "</version>");

				if (moduleInfo.description != null && !moduleInfo.description.trim().isEmpty())
				{
					stream.println("		<description><![CDATA[" + moduleInfo.description + "]]></description>");
				}

				if (moduleInfo.url != null && !moduleInfo.url.trim().isEmpty())
				{
					stream.println("		<url><![CDATA[" + moduleInfo.url + "]]></url>");
				}

				stream.println("		<taskdef>" + moduleInfo.taskdef + "</taskdef>");
				stream.println("		<taskdefJar>" + moduleInfo.taskdefJar + "</taskdefJar>");

				stream.println("		<libs>");

				for (String lib: moduleInfo.libs)
				{
					stream.println("			<lib>" + lib + "</lib>");
				}

				stream.println("		</libs>");

				if (moduleInfo.dependencies != null)
				{
					stream.println("		<dependencies>");

					for (String dep: moduleInfo.dependencies)
					{
						stream.println("			<module>" + dep + "</module>");
					}

					stream.println("		</dependencies>");
				}

				stream.println("	</module>");
			}

			stream.println("</modules>");

			stream.flush();
		}
		finally
		{
			try
			{
				stream.close();
				out.close();
			}
			catch (IOException e) {}
		}
	}

	private Map<String, ModuleInfo> getAllModules(Task task)
	{

		Map<String, ModuleInfo> modules = new HashMap<String, ModuleInfo>();

		for (String moduleDir : modulesRootDirs)
		{
			modules.putAll(ModuleHelper.getModulesInsideDir(moduleDir, task));
		}

		if (modules.isEmpty())
		{
			throw new BuildException("There are no modules found");
		}

		return modules;
	}

	private List<String> getInitialListOfModulesToBePackaged(Task task)
	{
		task.log("---> Getting initial list of modules to be packaged");

		File file = new File(frameworkBasedir + "/modules");
		if (!file.exists() || !file.isFile())
		{
			task.log("------> No modules specified - thus all previously found modules will be packed");
			return null;
		}

		List<String> modules = new LinkedList<String>();

		InputStream in = null;
		BufferedReader reader = null;

		try
		{
			int fileLength = (int)file.length();
			if (fileLength == 0)
			{
				task.log("------> No modules specified - thus all previously found modules will be packed");
				return null;
			}

			in = new FileInputStream(file);
			reader = new BufferedReader(new InputStreamReader(in));

			String line;

			while ((line = reader.readLine()) != null)
			{
				String moduleName = line.trim();

				if (!moduleName.isEmpty() && !moduleName.startsWith("#") && !modules.contains(moduleName))
				{
					modules.add(moduleName);
					task.log("------> Module '" + moduleName + "' specified to be packaged");
				}
    		}
		}
		catch (IOException e)
		{
			throw new BuildException("Failed read file: " + frameworkBasedir + "/modules", e);
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

		if (modules.isEmpty())
		{
			task.log("------> No modules specified - thus all previously found modules will be packed");
		}

		return modules.isEmpty() ? null : modules;
	}

	private Collection<ModuleInfo> getListOfModulesToBePackaged(Task task)
	{
		Map<String, ModuleInfo> allModules = getAllModules(task);
		Collection<String> modulesToPackage = getInitialListOfModulesToBePackaged(task);

		task.log("---> Resolving final list of modules to be packaged");

		modulesToPackage = resolveWholeListOfModulesToPackage(task, allModules, modulesToPackage);

		if (modulesToPackage == null || modulesToPackage.isEmpty())
		{
			throw new BuildException("There are no modules to be packaged");
		}

		Map<String, ModuleInfo> packagedModules = new LinkedHashMap<String, ModuleInfo>();

		while (!modulesToPackage.isEmpty())
		{
			List<ModuleInfo> newCandidatesToPackage = new LinkedList<ModuleInfo>();

			for (String moduleName : modulesToPackage)
			{
				if (canModuleBePackaged(allModules.get(moduleName), packagedModules))
				{
					newCandidatesToPackage.add(allModules.get(moduleName));
				}
			}

			for (ModuleInfo moduleInfo : newCandidatesToPackage)
			{
				if (!packagedModules.containsKey(moduleInfo.name))
				{
					packagedModules.put(moduleInfo.name, moduleInfo);
				}

				modulesToPackage.remove(moduleInfo.name);
			}
		}

		return packagedModules.values();
	}

	private boolean canModuleBePackaged(ModuleInfo moduleInfo, Map<String, ModuleInfo> packagedModules)
	{
		if (moduleInfo.dependencies == null || moduleInfo.dependencies.length == 0)
		{
			return true;
		}

		for (String dependency : moduleInfo.dependencies)
		{
			if (!packagedModules.containsKey(dependency))
			{
				return false;
			}
		}

		return true;
	}

	private Collection<String> resolveWholeListOfModulesToPackage(Task task, Map<String, ModuleInfo> allModules, Collection<String> initialModules)
	{
		if (initialModules == null || initialModules.isEmpty())
		{
			return allModules.keySet();
		}

		List<String> wholeList = new LinkedList<String>(initialModules);

		boolean valid = true;

		for (String moduleName : initialModules)
		{
			if (!allModules.containsKey(moduleName))
			{
				valid = false;
				task.log("------> [ERROR] Initially specified module '" + moduleName + "' can't be packaged, cause there are no such module");
			}

			if (!wholeList.contains(moduleName))
			{
				wholeList.add(moduleName);
			}

			List<String> dependencies = resolveAllModuleDependencies(task, moduleName, allModules, null, "------> ");
			for (String dependency : dependencies)
			{
				if (!wholeList.contains(moduleName))
				{
					wholeList.add(dependency);
				}
			}
		}

		if (!valid)
		{
			throw new BuildException("Some of specified modules could not be packaged, cause they don't exist");
		}

		return wholeList;
	}

	private List<String> resolveAllModuleDependencies(Task task, String moduleName, Map<String, ModuleInfo> allModules, List<String> resolvedDependencies, String indicator)
	{
		task.log(indicator + moduleName);

		List<String> dependencies = new LinkedList<String>();

		ModuleInfo moduleInfo = allModules.get(moduleName);
		if (moduleInfo.dependencies == null || moduleInfo.dependencies.length == 0)
		{
			return dependencies;
		}

		if (resolvedDependencies == null)
		{
			resolvedDependencies = new LinkedList<String>();
		}

		for (String dependency : moduleInfo.dependencies)
		{
			if (!resolvedDependencies.contains(dependency))
			{
				resolvedDependencies.add(dependency);
			}

			resolveAllModuleDependencies(task, dependency, allModules, resolvedDependencies, "---" + indicator);
		}

		return dependencies;
	}
}
