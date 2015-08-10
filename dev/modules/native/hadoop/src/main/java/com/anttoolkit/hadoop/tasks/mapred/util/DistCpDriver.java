package com.anttoolkit.hadoop.tasks.mapred.util;

import java.io.*;
import java.util.*;

import org.apache.hadoop.conf.*;
import org.apache.hadoop.fs.*;
import org.apache.hadoop.io.*;
import org.apache.hadoop.mapreduce.*;
import org.apache.hadoop.tools.*;
import org.apache.hadoop.tools.mapred.*;
import org.apache.hadoop.tools.util.*;
import org.apache.hadoop.util.*;
import org.apache.tools.ant.*;

import com.anttoolkit.general.tasks.*;

public class DistCpDriver implements Runnable
{
	private static final String DISTCP_DEFAULT_XML = "distcp-default.xml";
	private static final String PREFIX = "_distcp";
	private static final Random RANDOM = new Random(System.currentTimeMillis());
	private static final int SHUTDOWN_HOOK_PRIORITY = 30;
	private static final String WIP_PREFIX = "._WIP_";

	private Configuration conf;
	private DistCpOptions inputOptions;
	private GenericTask task;
	private Path metaFolder;
	private FileSystem jobFS;

	public DistCpDriver(GenericTask task, Configuration configuration, DistCpOptions inputOptions)
	{
		this.task = task;
		conf= new Configuration(configuration);
		conf.addResource(DISTCP_DEFAULT_XML);
		this.inputOptions = inputOptions;
	}

	@Override
	public void run()
	{
		cleanup();
	}

	public String execute(String jobName, boolean async)
	{
		metaFolder = createMetaFolderPath();

		try
		{
			jobFS = metaFolder.getFileSystem(conf);
		}
		catch (IOException e)
		{
			throw new BuildException("Failed to get DistCp meta folder filesystem", e);
		}

		Job job = null;
		boolean submitted = false;

		ShutdownHookManager.get().addShutdownHook(this, SHUTDOWN_HOOK_PRIORITY);

		try
		{
			job = createJob(jobName);
			createInputFileListing(job);
			job.submit();
			submitted = true;
		}
		catch (Exception e)
		{
			throw new BuildException("Failed to submit DistCp job", e);
		}
		finally
		{
			if (!submitted)
			{
				cleanup();
				ShutdownHookManager.get().removeShutdownHook(this);
			}
		}

		String jobId = job.getJobID().toString();
		job.getConfiguration().set(DistCpConstants.CONF_LABEL_DISTCP_JOB_ID, jobId);

		task.log("DistCp job " + jobId + " started");

		if (async)
		{
			return jobId;
		}

		boolean result;

		try
		{
			result = job.waitForCompletion(true);
		}
		catch (Exception e)
		{
			throw new BuildException("Failed to wait for completion of DistCp job " + jobId, e);
		}

		if (!result)
		{
			try
			{
				throw new BuildException("DistCp job " + jobId + " failed: " + job.getStatus().getFailureInfo());
			}
			catch (Exception e)
			{
				throw new BuildException("DistCp job " + jobId + " failed");
			}
		}

		task.log("DistCp job " + jobId + " completed");

		return jobId;
	}

	private Path createMetaFolderPath()
	{
		try
		{
			Path stagingDir = JobSubmissionFiles.getStagingDir(new Cluster(conf), conf);

			Path metaFolderPath = new Path(stagingDir, PREFIX + String.valueOf(RANDOM.nextInt()));
			conf.set(DistCpConstants.CONF_LABEL_META_FOLDER, metaFolderPath.toString());

			return metaFolderPath;
		}
		catch (Exception e)
		{
			throw new BuildException("Failed to create meta folder for DistCp job", e);
		}
	}

	private synchronized void cleanup()
	{
		if (metaFolder == null)
		{
			return;
		}

		try
		{
			jobFS.delete(metaFolder, true);
			metaFolder = null;
		}
		catch (IOException e)
		{
			task.log("Unable to cleanup DistCp meta folder: " + metaFolder, Project.MSG_ERR);
		}
	}

	private Job createJob(String jobName)
	{
		try
		{
			Job job = Job.getInstance(conf);
			job.setJobName(jobName);
			job.setInputFormatClass(DistCpUtils.getStrategy(conf, inputOptions));
			job.setJarByClass(CopyMapper.class);

			configureOutputFormat(job);

			job.setMapperClass(CopyMapper.class);
			job.setNumReduceTasks(0);
			job.setMapOutputKeyClass(Text.class);
			job.setMapOutputValueClass(Text.class);
			job.setOutputFormatClass(CopyOutputFormat.class);
			job.getConfiguration().set(JobContext.MAP_SPECULATIVE, "false");
			job.getConfiguration().set(JobContext.NUM_MAPS, String.valueOf(inputOptions.getMaxMaps()));

			if (inputOptions.getSslConfigurationFile() != null)
			{
				setupSSLConfig(job);
			}

			inputOptions.appendToConf(job.getConfiguration());

			return job;
		}
		catch (IOException e)
		{
			throw new BuildException("Failed to create DistCp job", e);
		}
	}

	private void configureOutputFormat(Job job)
			throws IOException
	{
		final Configuration jobConf = job.getConfiguration();
		Path targetPath = inputOptions.getTargetPath();
		FileSystem targetFS = targetPath.getFileSystem(jobConf);
		targetPath = targetPath.makeQualified(targetFS.getUri(), targetFS.getWorkingDirectory());

		if (inputOptions.shouldAtomicCommit())
		{
			Path workDir = inputOptions.getAtomicWorkPath();
			if (workDir == null)
			{
				workDir = targetPath.getParent();
			}

			workDir = new Path(workDir, WIP_PREFIX + targetPath.getName() + RANDOM.nextInt());
			FileSystem workFS = workDir.getFileSystem(jobConf);

			if (!DistCpUtils.compareFs(targetFS, workFS)) {
				throw new IllegalArgumentException("Work path " + workDir +
						" and target path " + targetPath + " are in different file system");
			}

			CopyOutputFormat.setWorkingDirectory(job, workDir);
		}
		else
		{
			CopyOutputFormat.setWorkingDirectory(job, targetPath);
		}

		CopyOutputFormat.setCommitDirectory(job, targetPath);

		Path logPath = inputOptions.getLogPath();
		if (logPath == null)
		{
			logPath = new Path(metaFolder, "_logs");
		}

		CopyOutputFormat.setOutputPath(job, logPath);
	}

	private void setupSSLConfig(Job job)
			throws IOException
	{
		Configuration configuration = job.getConfiguration();
		Path sslConfigPath = new Path(configuration.getResource(inputOptions.getSslConfigurationFile()).toString());

		addSSLFilesToDistCache(job, sslConfigPath);
		configuration.set(DistCpConstants.CONF_LABEL_SSL_CONF, sslConfigPath.getName());
		configuration.set(DistCpConstants.CONF_LABEL_SSL_KEYSTORE, sslConfigPath.getName());
	}

	private void addSSLFilesToDistCache(Job job, Path sslConfigPath)
			throws IOException
	{
		Configuration configuration = job.getConfiguration();
		FileSystem localFS = FileSystem.getLocal(configuration);

		Configuration sslConf = new Configuration(false);
		sslConf.addResource(sslConfigPath);

		Path localStorePath = getLocalStorePath(sslConf, DistCpConstants.CONF_LABEL_SSL_TRUST_STORE_LOCATION);
		job.addCacheFile(localStorePath.makeQualified(localFS.getUri(), localFS.getWorkingDirectory()).toUri());
		configuration.set(DistCpConstants.CONF_LABEL_SSL_TRUST_STORE_LOCATION, localStorePath.getName());

		localStorePath = getLocalStorePath(sslConf, DistCpConstants.CONF_LABEL_SSL_KEY_STORE_LOCATION);
		job.addCacheFile(localStorePath.makeQualified(localFS.getUri(), localFS.getWorkingDirectory()).toUri());
		configuration.set(DistCpConstants.CONF_LABEL_SSL_KEY_STORE_LOCATION, localStorePath.getName());

		job.addCacheFile(sslConfigPath.makeQualified(localFS.getUri(), localFS.getWorkingDirectory()).toUri());
	}

	private Path getLocalStorePath(Configuration sslConf, String storeKey)
			throws IOException
	{
		if (sslConf.get(storeKey) != null)
		{
			return new Path(sslConf.get(storeKey));
		}

		throw new IOException("Store for " + storeKey + " is not set in " + inputOptions.getSslConfigurationFile());
	}

	protected Path createInputFileListing(Job job)
	{
		try
		{
			Path fileListingPath = getFileListingPath();
			CopyListing copyListing = CopyListing.getCopyListing(job.getConfiguration(), job.getCredentials(), inputOptions);
			copyListing.buildListing(fileListingPath, inputOptions);
			return fileListingPath;
		}
		catch (IOException e)
		{
			throw new BuildException("Failed to create input file listing", e);
		}
	}

	protected Path getFileListingPath()
			throws IOException
	{
		String fileListPathStr = metaFolder + "/fileList.seq";
		Path path = new Path(fileListPathStr);
		return new Path(path.toUri().normalize().toString());
	}
}
