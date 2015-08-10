package com.anttoolkit.hadoop.tasks.hdfs;

import java.io.*;

import org.apache.hadoop.fs.*;
import org.apache.tools.ant.*;

import com.anttoolkit.hadoop.tasks.hadoop.*;
import com.anttoolkit.hadoop.tasks.hadoop.util.*;

public class CopyTask
		extends GenericHadoopTask
{
	private boolean overwrite = false;
	private String src;
	private String srcUser;
	private String srcPrincipal;
	private String srcKeytab;
	private String dest;
	private String destUser;
	private String destPrincipal;
	private String destKeytab;

	public void setOverwrite(boolean overwrite)
	{
		this.overwrite = overwrite;
	}

	public void setSrc(String src)
	{
		this.src = src;
	}

	public void setSrcUser(String srcUser)
	{
		this.srcUser = srcUser;
	}

	public void setSrcPrincipal(String principal)
	{
		this.srcPrincipal = principal;
	}

	public void setSrcKeytab(String keytab)
	{
		this.srcKeytab = keytab;
	}

	public void setDest(String dest)
	{
		this.dest = dest;
	}

	public void setDestUser(String destUser)
	{
		this.destUser = destUser;
	}

	public void setDestPrincipal(String principal)
	{
		this.destPrincipal = principal;
	}

	public void setDestKeytab(String keytab)
	{
		this.destKeytab = keytab;
	}

	@Override
	public void doHadoopWork() throws BuildException
	{
		try
		{
			FileUtil.copy(getSrcFileSystem(), new Path(src), getDestFileSystem(), new Path(dest), deleteSource(), overwrite, getSrcFileSystem().getConf());
		}
		catch (IOException e)
		{
			throw new BuildException("Failed to copy file '" + src + "' to '" + dest + "'", e);
		}
	}

	@Override
	protected void hadoopValidate()
	{
		if (src == null || src.trim().isEmpty())
		{
			throw new BuildException("Source file should be specified");
		}

		FileStatus status = getFileStatus(src, getSrcFileSystem());
		if (status == null)
		{
			throw new BuildException("There are no src file: " + src);
		}

		if (dest == null || dest.trim().isEmpty())
		{
			throw new BuildException("Destination should be specified");
		}
	}

	protected FileSystem getSrcFileSystem()
	{
		AuthenticationContext authContext = AuthenticationContext.instance(srcUser, srcPrincipal, srcKeytab);
		return authContext != null ? getRemoteFileSystem(authContext) : getRemoteFileSystem();
	}

	protected FileSystem getDestFileSystem()
	{
		AuthenticationContext authContext = AuthenticationContext.instance(destUser, destPrincipal, destKeytab);
		return authContext != null ? getRemoteFileSystem(authContext) : getRemoteFileSystem();
	}

	protected boolean deleteSource()
	{
		return false;
	}
}
