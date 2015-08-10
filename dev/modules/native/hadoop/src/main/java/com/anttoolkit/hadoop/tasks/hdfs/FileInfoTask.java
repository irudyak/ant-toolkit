package com.anttoolkit.hadoop.tasks.hdfs;

import java.text.*;
import java.util.*;

import org.apache.hadoop.fs.*;
import org.apache.tools.ant.*;

import com.anttoolkit.hadoop.tasks.hadoop.*;

public class FileInfoTask
		extends GenericHadoopTask
{
	private String timeFormat = "MM/dd/yyyy hh:mm:ss aaa";
	private String locale = Locale.US.toString();

	private String file;
	private String typeProperty;
	private String ownerProperty;
	private String groupProperty;
	private String lengthProperty;
	private String replicationProperty;
	private String blockSizeProperty;
	private String accessTimeProperty;
	private String modificationTimeProperty;
	private String permissionsProperty;

	public void setTimeFormat(String format)
	{
		timeFormat = format;
	}

	public void setLocale(String locale)
	{
		this.locale = locale;
	}

	public void setFile(String file)
	{
		this.file = file;
	}

	public void setTypeProperty(String property)
	{
		this.typeProperty = property;
	}

	public void setOwnerProperty(String property)
	{
		this.ownerProperty = property;
	}

	public void setGroupProperty(String property)
	{
		this.groupProperty = property;
	}

	public void setLengthProperty(String property)
	{
		this.lengthProperty = property;
	}

	public void setReplicationProperty(String property)
	{
		this.replicationProperty = property;
	}

	public void setBlockSizeProperty(String property)
	{
		this.blockSizeProperty = property;
	}

	public void setAccessTimeProperty(String property)
	{
		this.accessTimeProperty = property;
	}

	public void setModificationTimeProperty(String property)
	{
		this.modificationTimeProperty = property;
	}

	public void setPermissionsProperty(String property)
	{
		this.permissionsProperty = property;
	}

	@Override
	public void doHadoopWork() throws BuildException
	{
		FileStatus status = getFileStatus(file);
		if (status == null)
		{
			throw new IllegalArgumentException("Specified file doesn't exist: " + file);
		}

		if (typeProperty != null)
		{
			setPropertyThreadSafe(typeProperty, status.isDirectory() ? "dir" : "file");
		}

		if (ownerProperty != null)
		{
			setPropertyThreadSafe(ownerProperty, status.getOwner());
		}

		if (groupProperty != null)
		{
			setPropertyThreadSafe(groupProperty, status.getGroup());
		}

		if (lengthProperty != null)
		{
			setPropertyThreadSafe(lengthProperty, Long.toString(status.getLen()));
		}

		if (replicationProperty != null)
		{
			setPropertyThreadSafe(replicationProperty, Short.toString(status.getReplication()));
		}

		if (blockSizeProperty != null)
		{
			setPropertyThreadSafe(blockSizeProperty, Long.toString(status.getBlockSize()));
		}

		if (accessTimeProperty != null)
		{
			setPropertyThreadSafe(accessTimeProperty, getDateFormat().format(status.getAccessTime()));
		}

		if (modificationTimeProperty != null)
		{
			setPropertyThreadSafe(modificationTimeProperty, getDateFormat().format(status.getModificationTime()));
		}

		if (permissionsProperty != null)
		{
			setPropertyThreadSafe(permissionsProperty, status.getPermission().toString());
		}
	}

	@Override
	protected void hadoopValidate()
	{
		if (file == null || file.trim().isEmpty())
		{
			throw new BuildException("File cant be null");
		}
	}

	private DateFormat getDateFormat()
	{
		try
		{
			return new SimpleDateFormat(timeFormat, new Locale(locale));
		}
		catch (Throwable e)
		{
			throw new BuildException("Incorrect time format/locale specified: " + timeFormat + "/" + locale, e);
		}
	}
}
