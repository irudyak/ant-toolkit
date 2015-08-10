package com.anttoolkit.hadoop.tasks.mapred;

import java.io.*;
import java.util.*;

import org.apache.hadoop.fs.*;
import org.apache.hadoop.mapred.*;
import org.apache.hadoop.mapreduce.*;
import org.apache.tools.ant.*;

import com.anttoolkit.hadoop.tasks.hadoop.*;
import com.anttoolkit.hadoop.tasks.mapred.util.*;
import com.anttoolkit.general.common.*;

public class CreateJobTask extends GenericHadoopTask
{
	private String jobName;
	private String reference;
	private String jobConfigFile;
	private String jobJar;
	private Class jobJarByClass;
	private List<NameValueHolder> properties = new LinkedList<NameValueHolder>();
	private List<String> cacheFiles = new LinkedList<String>();
	private List<String> cacheArchives = new LinkedList<String>();
	private List<String> classpathFiles = new LinkedList<String>();
	private List<String> classpathArchives = new LinkedList<String>();
	private List<InputPath> inputsPaths = new LinkedList<InputPath>();

	public void setJobName(String name)
	{
		this.jobName = name;
	}

	public void setReference(String reference)
	{
		this.reference = reference;
	}

	public void setJobFile(String file)
	{
		jobConfigFile = file;
	}

	public void addConfiguredJobJar(ValueHolder value)
	{
		this.jobJar = getFileFullPath(value.getValue());
	}

	public void addConfiguredJobJarByClass(ValueHolder value)
	{
		try
		{
			this.jobJarByClass = ReflectionHelper.forName(value.getValue());
		}
		catch (ClassNotFoundException e)
		{
			throw new BuildException("Failed to find java class " + value.getValue());
		}
	}

	public void addConfiguredProperty(NameValueHolder property)
	{
		properties.add(property);
	}

	public void addConfiguredAddCacheFile(ValueHolder value)
	{
		cacheFiles.add(getFileFullPath(value.getValue()));
	}

	public void addConfiguredAddCacheArchive(ValueHolder value)
	{
		cacheArchives.add(getFileFullPath(value.getValue()));
	}

	public void addConfiguredAddClasspathFile(ValueHolder value)
	{
		classpathFiles.add(getFileFullPath(value.getValue()));
	}

	public void addConfiguredAddClasspathArchive(ValueHolder value)
	{
		classpathArchives.add(getFileFullPath(value.getValue()));
	}

	public void addConfiguredInputPath(InputPath input)
	{
		inputsPaths.add(input);
	}

	@Override
	protected void doHadoopWork() throws BuildException
	{
		JobConf conf = jobConfigFile != null ?
				new JobConf(getFileFullPath(jobConfigFile)) :
				new JobConf(getConfiguration());

		for (NameValueHolder prop : properties)
		{
			conf.set(prop.getName(), prop.getValue());
		}

		Job job;

		try
		{
			job = jobName != null ? Job.getInstance(conf, jobName) : Job.getInstance(conf);
		}
		catch (IOException e)
		{
			throw new BuildException("Failed to create job object");
		}

		if (jobJar != null)
		{
			job.setJar(jobJar);
		}

		if (jobJarByClass != null)
		{
			job.setJarByClass(jobJarByClass);
		}

		for (String file : cacheFiles)
		{
			job.addCacheFile((new Path(file)).toUri());
		}

		for (String file : cacheArchives)
		{
			job.addCacheArchive((new Path(file)).toUri());
		}

		for (InputPath inputPath : inputsPaths)
		{
			inputPath.addInputPath(job);
		}

		for (String file : classpathFiles)
		{
			try
			{
				job.addFileToClassPath(new Path(file));
			}
			catch (IOException e)
			{
				throw new BuildException("Failed to add '" + file + "' file to Hadoop job CLASSPATH");
			}
		}

		for (String file : classpathArchives)
		{
			try
			{
				job.addArchiveToClassPath(new Path(file));
			}
			catch (IOException e)
			{
				throw new BuildException("Failed to add '" + file + "' archive to Hadoop job CLASSPATH");
			}
		}

		this.setReference(reference, job);
	}

	@Override
	protected void hadoopValidate()
	{
		if (reference == null || reference.trim().isEmpty())
		{
			throw new BuildException("Job reference should be specified");
		}
	}
}
