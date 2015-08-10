package com.anttoolkit.hadoop.tasks.yarn;

import java.io.*;
import java.lang.reflect.*;
import java.net.*;
import java.util.*;

import org.apache.hadoop.fs.*;
import org.apache.hadoop.util.*;
import org.apache.tools.ant.*;

import com.anttoolkit.general.common.*;

public class RunJarTask extends GenericYarnTask
{
	private String jarFile;
	private String mainClassName;
	private ArrayList<String> args = new ArrayList<String>();

	private boolean useClientClassloader = false;
	private String clientClassloaderSystemClasses;
	private String hadoopClasspath;

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

	public void setUseClientClassloader(boolean useClientClassloader)
	{
		this.useClientClassloader = useClientClassloader;
	}

	public void setClientClassloaderSystemClasses(String classes)
	{
		this.clientClassloaderSystemClasses = classes;
	}

	public void setHadoopClasspath(String classpath)
	{
		this.hadoopClasspath = classpath;
	}

	@Override
	protected void doHadoopWork() throws BuildException
	{
		File tmpDir = new File(System.getProperty("java.io.tmpdir"));
		ensureDirectory(tmpDir);

		File workDir;
		String workDirPath;

		try
		{
			workDir = File.createTempFile("hadoop-unjar", "", tmpDir);
			workDirPath = workDir.getCanonicalPath();
		}
		catch (IOException e)
		{
			// If user has insufficient perms to write to tmpDir, default
			// "Permission denied" message doesn't specify a filename.
			throw new BuildException("Error creating temp dir in java.io.tmpdir " + tmpDir, e);
		}

		if (!workDir.delete())
		{
			throw new BuildException("Delete failed for " + workDir);
		}

		ensureDirectory(workDir);

		ClassLoader oldClassLoader = null;

		try
		{
			JarHelper.unJar(jarFile, workDirPath);
			ClassLoader loader = createClassLoader(new File(jarFile), workDir);

			oldClassLoader = Thread.currentThread().getContextClassLoader();

			Thread.currentThread().setContextClassLoader(loader);
			Class<?> mainClass = Class.forName(mainClassName, true, loader);
			Method main = mainClass.getMethod("main", new Class[] {Array.newInstance(String.class, 0).getClass()});

			main.invoke(null, new Object[] {args.toArray(new String[0])});
		}
		catch (ClassNotFoundException e)
		{
			throw new BuildException("Failed to find java class \"" + mainClassName + "\" inside jar: " + jarFile, e);
		}
		catch (NoSuchMethodException e)
		{
			throw new BuildException("Failed to find main method for java class \"" + mainClassName + "\" inside jar: " + jarFile, e);
		}
		catch (MalformedURLException e)
		{
			throw new BuildException("Failed to create class loader based on jar file content: " + jarFile, e);
		}
		catch (IllegalAccessException e)
		{
			throw new BuildException("Access exception occured while trying to execute main method of the class: " + mainClassName, e);
		}
		catch (InvocationTargetException e)
		{
			throw new BuildException("Exception occured during main method execution of the class: " + mainClassName, e);
		}
		finally
		{
			if (oldClassLoader != null)
			{
				Thread.currentThread().setContextClassLoader(oldClassLoader);
			}

			FileUtil.fullyDelete(workDir);
		}
	}

	@Override
	protected void hadoopValidate()
	{
		if (jarFile == null || jarFile.trim().isEmpty())
		{
			throw new BuildException("Jar file should be specified");
		}

		mainClassName = getJarMainClass();
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

	private void ensureDirectory(File dir)
	{
		if (!dir.mkdirs() && !dir.isDirectory())
		{
			throw new BuildException("Mkdirs failed to create " + dir.toString());
		}
	}

	private ClassLoader createClassLoader(File file, final File workDir)
			throws MalformedURLException
	{
		// see if the client classloader is enabled
		if (useClientClassloader)
		{
			StringBuilder sb = new StringBuilder();
			sb.append(workDir).append("/").
					append(File.pathSeparator).append(file).
					append(File.pathSeparator).append(workDir).append("/classes/").
					append(File.pathSeparator).append(workDir).append("/lib/*");

			// HADOOP_CLASSPATH is added to the client classpath
			if (hadoopClasspath != null && !hadoopClasspath.isEmpty())
			{
				sb.append(File.pathSeparator).append(hadoopClasspath);
			}

			String clientClasspath = sb.toString();

			// get the system classes
			List<String> systemClassesList = clientClassloaderSystemClasses == null ?
					null :
					Arrays.asList(StringUtils.getTrimmedStrings(clientClassloaderSystemClasses));

			// create an application classloader that isolates the user classes
			return new ApplicationClassLoader(clientClasspath, getClass().getClassLoader(), systemClassesList);
		}

		List<URL> classPath = new ArrayList<URL>();
		classPath.add(new File(workDir + "/").toURI().toURL());
		classPath.add(file.toURI().toURL());
		classPath.add(new File(workDir, "classes/").toURI().toURL());
		File[] libs = new File(workDir, "lib").listFiles();

		if (libs != null)
		{
			for (File lib : libs)
			{
				classPath.add(lib.toURI().toURL());
			}
		}

		// create a normal parent-delegating classloader
		return new URLClassLoader(classPath.toArray(new URL[0]));
	}
}
