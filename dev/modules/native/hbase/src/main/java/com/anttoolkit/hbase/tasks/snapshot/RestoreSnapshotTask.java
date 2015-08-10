package com.anttoolkit.hbase.tasks.snapshot;

import com.anttoolkit.hbase.tasks.*;
import org.apache.tools.ant.*;

public class RestoreSnapshotTask extends GenericHBaseTask
{
	private String snapshot;

	public void setSnapshot(String snapshot)
	{
		this.snapshot = snapshot;
	}

	@Override
	protected void doHadoopWork() throws BuildException
	{
		try
		{
			getHBaseAdmin().restoreSnapshot(snapshot);
		}
		catch (Exception e)
		{
			throw new BuildException("Failed to restore snapshot " + snapshot);
		}
	}

	protected void hadoopValidate()
	{
		if (snapshot == null || snapshot.trim().isEmpty())
		{
			throw new BuildException("Snapshot to restore should be specified");
		}
	}
}
