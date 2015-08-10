package com.anttoolkit.hadoop.tasks.mapred;

import java.io.*;
import java.util.*;

import org.apache.hadoop.fs.*;
import org.apache.hadoop.tools.*;
import org.apache.tools.ant.*;

import com.anttoolkit.hadoop.tasks.hadoop.*;
import com.anttoolkit.hadoop.tasks.mapred.util.*;

public class DistCpTask extends GenericHadoopTask
{
	private static final String JOB_DEFAULT_NAME = "distcp";

	private String preserve;
	private Boolean ignoreFailures;
	private String logdir;
	private Integer maps;
	private Boolean overwrite;
	private Boolean syncFolders;
	private Boolean delete;
	private String strategy;
	private Integer bandwidth;
	private Boolean atomic;
	private String tempDir;
	private String sslConf;
	private boolean asynch = false;
	private String srcListing;
	private String src;
	private String dest;
	private String jobName;
	private String jobIdProperty;

	public void setPreserve(String preserve)
	{
		this.preserve = preserve;
	}

	public void setIgnoreFailures(boolean ignore)
	{
		ignoreFailures = ignore;
	}

	public void setLogdir(String logdir)
	{
		this.logdir = logdir;
	}

	public void setMaps(int maps)
	{
		this.maps = maps;
	}

	public void setOverwrite(boolean overwrite)
	{
		this.overwrite = overwrite;
	}

	public void setSyncFolders(boolean syncFolders)
	{
		this.syncFolders = syncFolders;
	}

	public void setDelete(boolean delete)
	{
		this.delete = delete;
	}

	public void setStrategy(String strategy)
	{
		this.strategy = strategy;
	}

	public void setBandwidth(int bandwidth)
	{
		this.bandwidth = bandwidth;
	}

	public void setAtomic(boolean atomic)
	{
		this.atomic = atomic;
	}

	public void setTempDir(String tempDir)
	{
		this.tempDir = tempDir;
	}

	public void setSslConf(String sslConf)
	{
		this.sslConf = sslConf;
	}

	public void setAsynch(boolean asynch)
	{
		this.asynch = asynch;
	}

	public void setSrcListing(String srcListing)
	{
		this.srcListing = srcListing;
	}

	public void setSrc(String src)
	{
		this.src = src;
	}

	public void setDest(String dest)
	{
		this.dest = dest;
	}

	public void setJobName(String jobName)
	{
		this.jobName = jobName;
	}

	public void setJobIdProperty(String jobIdProperty)
	{
		this.jobIdProperty = jobIdProperty;
	}

	@Override
	protected void doHadoopWork() throws BuildException
	{
		DistCpDriver driver = new DistCpDriver(this, getConfiguration(), getOptions());
		String jobId = driver.execute(jobName == null ? JOB_DEFAULT_NAME : jobName, asynch);

		if (jobIdProperty != null)
		{
			this.setPropertyThreadSafe(jobIdProperty, jobId);
		}
	}

	@Override
	protected void hadoopValidate()
	{
		if ((src == null || src.trim().isEmpty()) &&
			(srcListing == null || srcListing.trim().isEmpty()))
		{
			throw new BuildException("No source specified for DistCp job");
		}

		if (dest == null || dest.trim().isEmpty())
		{
			throw new BuildException("No target specified for DistCp job");
		}

		if (tempDir != null && !tempDir.trim().isEmpty() &&
			(atomic == null || !atomic))
		{
			throw new BuildException("tempDir option only works in pair with atomic=true option");
		}

		if (maps != null && maps <= 0)
		{
			throw new BuildException("Incorrect number of maps specified: " + maps);
		}

		if (sslConf != null && !sslConf.trim().isEmpty())
		{
			sslConf = getFileFullPath(sslConf);

			File file = new File(sslConf);
			if (!file.exists() || !file.isFile())
			{
				throw new BuildException("SSL config file '" + sslConf + "' doesn't exist");
			}
		}
	}

	private DistCpOptions getOptions()
	{
		List<Path> srcPaths = new LinkedList<Path>();
		if (src != null && !src.trim().isEmpty())
		{
			String[] paths = src.split(",", -1);
			for (String path : paths)
			{
				srcPaths.add(new Path(path));
			}
		}


		DistCpOptions options = srcListing != null && !srcListing.trim().isEmpty() ?
				new DistCpOptions(new Path(srcListing), new Path(dest)) :
				new DistCpOptions(srcPaths, new Path(dest));

		if (preserve != null)
		{
			for (int i = 0; i < preserve.length(); i++)
			{
				options.preserve(DistCpOptions.FileAttribute.getAttribute(preserve.charAt(i)));
			}
		}

		if (ignoreFailures != null)
		{
			options.setIgnoreFailures(ignoreFailures);
		}

		if (logdir != null)
		{
			options.setLogPath(new Path(logdir));
		}

		if (maps != null)
		{
			options.setMaxMaps(maps);
		}

		if (overwrite != null)
		{
			options.setOverwrite(overwrite);
		}

		if (syncFolders != null)
		{
			options.setSyncFolder(syncFolders);
		}

		if (delete != null)
		{
			options.setDeleteMissing(delete);
		}

		if (strategy != null)
		{
			options.setCopyStrategy(strategy);
		}

		if (bandwidth != null)
		{
			options.setMapBandwidth(bandwidth);
		}

		if (atomic != null)
		{
			options.setAtomicCommit(atomic);
		}

		if (tempDir != null)
		{
			options.setAtomicWorkPath(new Path(tempDir));
		}

		if (sslConf != null)
		{
			options.setSslConfigurationFile(sslConf);
		}

		return options;
	}
}
