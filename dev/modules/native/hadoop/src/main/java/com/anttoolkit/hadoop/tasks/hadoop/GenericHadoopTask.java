package com.anttoolkit.hadoop.tasks.hadoop;

import java.io.*;
import java.security.*;
import java.util.*;

import org.apache.commons.logging.LogFactory;
import org.apache.hadoop.conf.*;
import org.apache.hadoop.fs.*;
import org.apache.hadoop.fs.Path;
import org.apache.hadoop.mapreduce.*;
import org.apache.hadoop.security.*;
import org.apache.hadoop.security.AccessControlException;
import org.apache.tools.ant.*;

import com.anttoolkit.general.tasks.*;
import com.anttoolkit.hadoop.types.*;
import com.anttoolkit.hadoop.tasks.hadoop.util.*;

public abstract class GenericHadoopTask
		extends GenericTask
{
	public static final int DEFAULT_IO_LENGTH = 1024 * 1024;

	private static final String DEFAULT_FILESYSTEM_KEY = "_";

	private HadoopConfig hadoopConf;
	private String hadoopUser;
	private String principal;
	private String keytab;
	private String defaultFS;
	private HadoopSessionClassLoader hadoopSessionClassLoader;
	private boolean authContextEstablished = false;

	private Map<String, FileSystem> remoteFileSystems = new HashMap<String, FileSystem>();
	private LocalFileSystem localFileSystem;
	private Cluster cluster;

	private String defaultFsPrefix;

	public static void processDirectoryFiles(FileProcessor processor, FileSystem fs, String dir, boolean recursive)
	{
		processDirectoryFiles(processor, fs, dir, recursive, true);
	}

	public void setHadoopConfig(String ref)
	{
		Object obj = getReference(ref);
		if (!(obj instanceof HadoopConfig))
		{
			throw new IllegalArgumentException("Incorrect Hadoop config specified");
		}

		this.hadoopConf = (HadoopConfig)obj;
	}

	public void setHadoopUser(String user)
	{
		hadoopUser = user == null || user.trim().isEmpty() ? null : user.trim();
	}

	public void setPrincipal(String principal)
	{
		this.principal = principal == null || principal.trim().isEmpty() ? null : principal.trim();
	}

	public void setKeytab(String keytab)
	{
		this.keytab = keytab == null || keytab.trim().isEmpty() ? null : keytab.trim();
	}

	public void setDefaultFS(String fs)
	{
		defaultFS = fs == null ? null : fs.trim();
		defaultFS = defaultFS == null || defaultFS.isEmpty() ? null : defaultFS;
	}

	@Override
	public final void doWork() throws BuildException
	{
		setHadoopContext();

		try
		{
			AuthenticationContext authContext = HadoopContextManager.getAuthenticationContext();
			if (authContext == null)
			{
				hadoopValidate();
				doHadoopWork();
				return;
			}

			executePrivilegedAction(authContext, new PrivilegedExceptionAction<Void>()
			{
				public Void run() throws Exception
				{
					hadoopValidate();
					doHadoopWork();
					return null;
				}
			});
		}
		finally
		{
			releaseHadoopResources();
			releaseHadoopContext();
		}
	}

	@Override
	protected final void validate()
	{
	}

	protected abstract void doHadoopWork() throws BuildException;

	protected void hadoopValidate()
	{
	}

	protected List<File> provideExtraResourcesToHadoopClassLoader()
	{
		return null;
	}

	protected boolean substituteClassLoader()
	{
		return true;
	}

	protected FileStatus getFileStatus(String file)
	{
		return getFileStatus(file, getRemoteFileSystem());
	}

	protected FileStatus getFileStatus(String file, FileSystem fileSystem)
	{
		if (fileSystem == null)
		{
			throw new IllegalArgumentException("File system can't be null");
		}

		try
		{
			return fileSystem.getFileStatus(new Path(file));
		}
		catch (FileNotFoundException e)
		{
			return null;
		}
		catch (IOException e)
		{
			throw new BuildException("Failed to get status for file: " + file, e);
		}
	}

	protected LocalFileSystem getLocalFileSystem()
	{
		if (localFileSystem != null)
		{
			return localFileSystem;
		}

		try
		{
			return localFileSystem = FileSystem.getLocal(getConfiguration());
		}
		catch (IOException e)
		{
			throw new BuildException("Failed to get LocalFileSystem", e);
		}
	}

	protected FileSystem getRemoteFileSystem()
	{
		try
		{
			AuthenticationContext authContext = HadoopContextManager.getAuthenticationContext();
			String key;

			if (authContext != null)
			{
				key = authContext.toString();
			}
			else
			{
				UserGroupInformation ugi = UserGroupInformation.getLoginUser();
				String currentUser = ugi == null ? null : ugi.getUserName();
				key = currentUser != null ? currentUser : DEFAULT_FILESYSTEM_KEY;
			}

			if (remoteFileSystems.containsKey(key))
			{
				return remoteFileSystems.get(key);
			}

			FileSystem fileSystem = authContext == null ?
					FileSystem.get(getConfiguration()) :
					executePrivilegedAction(authContext, new PrivilegedExceptionAction<FileSystem>()
						{
							public FileSystem run() throws Exception
							{
								return FileSystem.get(getConfiguration());
							}
						});

			remoteFileSystems.put(key, fileSystem);

			return fileSystem;
		}
		catch (IOException e)
		{
			throw new BuildException("Failed to get remote FileSystem", e);
		}
	}

	protected FileSystem getRemoteFileSystem(AuthenticationContext authContext)
	{
		if (authContext == null)
		{
			return getRemoteFileSystem();
		}

		if (remoteFileSystems.containsKey(authContext.toString()))
		{
			return remoteFileSystems.get(authContext.toString());
		}

		FileSystem filesystem = executePrivilegedAction(authContext, new PrivilegedExceptionAction<FileSystem>()
		{
			public FileSystem run() throws Exception
			{
				return FileSystem.get(getConfiguration());
			}
		});

		remoteFileSystems.put(authContext.toString(), filesystem);

		return filesystem;
	}

	protected Configuration getConfiguration()
	{
		Configuration conf = new Configuration();

		if (getDefaultFS() != null)
		{
			conf.set(HadoopConfig.HADOOP_DEFAULT_FS_PROP, getDefaultFS());
		}

		if (hadoopConf != null)
		{
			for (Property prop : hadoopConf.getProperties())
			{
				conf.set(prop.getName(), prop.getValue());
			}
		}

		return conf;
	}

	protected String getRemoteFilePath(Path path)
	{
		String _path = path.toString().trim();

		if (getDefaultFilesystemPrefix() != null)
		{
			_path = _path.replace(getDefaultFilesystemPrefix(), "").trim();
		}
		else
		{
			int index = _path.indexOf("//");
			index = index == -1 ? -1 : _path.indexOf("/", index + 2);
			_path = index == -1 ? _path.trim() : _path.substring(index);
		}

		return _path.startsWith("/") ? _path : "/" + _path;
	}

	protected Job getJob(String jobId)
	{
		try
		{
			return getCluster().getJob(JobID.forName(jobId));
		}
		catch (IOException e)
		{
			throw new BuildException("Failed to obtain job " + jobId + " from cluster");
		}
		catch (InterruptedException e)
		{
			throw new BuildException("Failed to obtain job " + jobId + " from cluster");
		}
	}

	protected JobStatus getJobStatus(String jobId)
	{
		try
		{
			Job job = getJob(jobId);
			if (job != null)
			{
				return job.getStatus();
			}

			JobID id = JobID.forName(jobId);

			JobStatus[] statuses = getCluster().getAllJobStatuses();
			for (JobStatus status : statuses)
			{
				if (status.getJobID().equals(id))
				{
					return status;
				}
			}
		}
		catch (IOException e)
		{
			throw new BuildException("Failed to get status of the job " + jobId, e);
		}
		catch (InterruptedException e)
		{
			throw new BuildException("Failed to get status of the job " + jobId, e);
		}

		return null;
	}

	protected JobStatus.State getJobState(String jobId)
	{
		JobStatus status = getJobStatus(jobId);
		return status == null ? null : status.getState();
	}

	protected Cluster getCluster()
	{
		if (cluster != null)
		{
			return cluster;
		}

		try
		{
			return cluster = new Cluster(getConfiguration());
		}
		catch (IOException e)
		{
			throw new BuildException("Failed to get instance of Hadoop cluster", e);
		}
	}

	protected void setHadoopContext()
	{
		if (hadoopConf != null)
		{
			HadoopContextManager.setConfigContext(hadoopConf);
		}

		if (hadoopUser != null ||
			(hadoopConf != null && hadoopConf.getHadoopUser() != null) ||
			(principal != null && keytab != null) ||
			(hadoopConf != null && hadoopConf.getPrincipal() != null && hadoopConf.getKeytab() != null))
		{
			AuthenticationContext authContext = AuthenticationContext.instance(hadoopUser, principal, keytab, hadoopConf);
			if (authContext != null && !authContext.equals(HadoopContextManager.getAuthenticationContext()))
			{
				HadoopContextManager.setAuthenticationContext(authContext);
				authContextEstablished = true;
			}
		}

		if (defaultFS != null)
		{
			HadoopContextManager.setFilesystemContext(defaultFS);
		}

		if (substituteClassLoader())
		{
			if (HadoopContextManager.getConfigContext() != null)
			{
				hadoopSessionClassLoader = new HadoopSessionClassLoader(HadoopContextManager.getConfigContext(), provideExtraResourcesToHadoopClassLoader());
				hadoopSessionClassLoader.substituteThreadClassLoader();
			}
		}
	}

	protected void releaseHadoopContext()
	{
		if (hadoopConf != null)
		{
			HadoopContextManager.releaseConfigContext();
		}

		if (authContextEstablished)
		{
			HadoopContextManager.releaseAuthenticationContext();
			authContextEstablished = false;
		}

		if (defaultFS != null)
		{
			HadoopContextManager.releaseFilesystemContext();
		}

		if (substituteClassLoader() && hadoopSessionClassLoader != null)
		{
			hadoopSessionClassLoader.restoreThreadClassLoader();
		}
	}

	protected void releaseHadoopResources()
	{
		if (localFileSystem != null)
		{
			try
			{
				localFileSystem.close();
			}
			catch (IOException e) {}
		}

		if (!remoteFileSystems.isEmpty())
		{
			try
			{
				for (FileSystem fs : remoteFileSystems.values())
				{
					fs.close();
				}
			}
			catch (IOException e) {}
		}

		if (cluster != null)
		{
			try
			{
				cluster.close();
			}
			catch (IOException e) {}
		}

		remoteFileSystems.clear();
		localFileSystem = null;
	}

	protected String getCurrentHadoopUser()
	{
		AuthenticationContext authContext = HadoopContextManager.getAuthenticationContext();
		if (authContext != null)
		{
			return authContext.getHadoopUser(getProject(), getConfiguration());
		}

		try
		{
			return UserGroupInformation.getCurrentUser().getShortUserName();
		}
		catch (IOException e)
		{
			throw new BuildException("Failed to get current Hadoop user short name", e);
		}
	}

	private static void processDirectoryFiles(FileProcessor processor, FileSystem fs, String dir, boolean recursive, boolean isRootDir)
	{
		if (processor == null)
		{
			throw new IllegalArgumentException("File processor should be specified");
		}

		if (fs == null)
		{
			throw new IllegalArgumentException("File system should be specified");
		}

		if (dir == null || dir.trim().isEmpty())
		{
			throw new IllegalArgumentException("Directory should be specified");
		}

		try
		{
			FileStatus[] statuses = fs.listStatus(new Path(dir));
			for (FileStatus status : statuses)
			{
				processor.process(status);

				if (status.isDirectory() && recursive)
				{
					processDirectoryFiles(processor, fs, status.getPath().toString(), true, false);
				}
			}
		}
		catch (FileNotFoundException e)
		{
			throw new BuildException("Specified directory doesn't exist: " + dir, e);
		}
		catch (AccessControlException e)
		{
			if (isRootDir)
			{
				throw new BuildException("Not enough rights to list files of directory: " + dir, e);
			}

			System.out.println("Not enough rights to list files of directory: " + dir);
		}
		catch (IOException e)
		{
			throw new BuildException("Failed to get directory files: " + dir, e);
		}
	}

	private <T> T executePrivilegedAction(AuthenticationContext authContext, PrivilegedExceptionAction<T> action)
	{
		AuthenticationContext currentAuthContext = HadoopContextManager.getAuthenticationContext();

		//prevents creation of several absolutely the same security contexts for privileged action execution
		if (authContext.equals(currentAuthContext))
		{
			return currentAuthContext.executePrivilegedAction(this.getProject(), getConfiguration(), action);
		}
		else
		{
			return authContext.executePrivilegedAction(this.getProject(), getConfiguration(), action);
		}
	}

	private String getDefaultFilesystemPrefix()
	{
		if (defaultFsPrefix != null)
		{
			return defaultFsPrefix.isEmpty() ? null : defaultFsPrefix;
		}

		defaultFsPrefix = getConfiguration().get(HadoopConfig.HADOOP_DEFAULT_FS_PROP);
		defaultFsPrefix = defaultFsPrefix == null || defaultFsPrefix.trim().isEmpty() ? "" : defaultFsPrefix.trim();

		return defaultFsPrefix.isEmpty() ? null : defaultFsPrefix;
	}

	private String getDefaultFS()
	{
		String fs = defaultFS != null ? defaultFS : HadoopContextManager.getFilesystemContext();
		return fs != null ?
				fs :
				HadoopContextManager.getConfigContext() != null ?
				HadoopContextManager.getConfigContext().getConfiguredPropertyValue(HadoopConfig.HADOOP_DEFAULT_FS_PROP) :
				null;
	}
}
