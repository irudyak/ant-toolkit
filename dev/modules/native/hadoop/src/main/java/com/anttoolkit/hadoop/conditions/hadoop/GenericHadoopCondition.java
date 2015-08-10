package com.anttoolkit.hadoop.conditions.hadoop;

import java.io.*;
import java.security.*;

import org.apache.hadoop.conf.*;
import org.apache.hadoop.fs.*;

import org.apache.tools.ant.taskdefs.condition.*;
import org.apache.tools.ant.*;

import com.anttoolkit.hadoop.tasks.hadoop.util.*;
import com.anttoolkit.hadoop.types.*;

public abstract class GenericHadoopCondition
		implements Condition
{
	@Override
	public final boolean eval() throws BuildException
	{
		validate();

		HadoopSessionClassLoader hadoopSessionClassLoader = null;

		if (HadoopContextManager.getConfigContext() != null)
		{
			hadoopSessionClassLoader = new HadoopSessionClassLoader(HadoopContextManager.getConfigContext(), null);
			hadoopSessionClassLoader.substituteThreadClassLoader();
		}

		try
		{
			AuthenticationContext authContext = HadoopContextManager.getAuthenticationContext();
			if (authContext == null)
			{
				return evaluate();
			}

			return authContext.executePrivilegedAction(null, getConfiguration(), new PrivilegedExceptionAction<Boolean>()
				{
					@Override
					public Boolean run() throws IOException
					{
						return evaluate();
					}
				});
		}
		finally
		{
			if (hadoopSessionClassLoader != null)
			{
				hadoopSessionClassLoader.restoreThreadClassLoader();
			}
		}
	}

	protected abstract boolean evaluate() throws BuildException;

	protected void validate()
	{
	}

	protected FileSystem getRemoteFileSystem()
	{
		try
		{
			return FileSystem.get(getConfiguration());
		}
		catch (IOException e)
		{
			throw new BuildException("Failed to get FileSystem", e);
		}
	}

	protected Configuration getConfiguration()
	{
		Configuration conf = new Configuration();

		String defaultFS = HadoopContextManager.getFilesystemContext();
		defaultFS = defaultFS != null ?
				defaultFS :
				HadoopContextManager.getConfigContext() != null ?
				HadoopContextManager.getConfigContext().getConfiguredPropertyValue(HadoopConfig.HADOOP_DEFAULT_FS_PROP) :
				null;

		if (defaultFS != null)
		{
			conf.set(HadoopConfig.HADOOP_DEFAULT_FS_PROP, defaultFS);
		}

		return conf;
	}
}
