package com.anttoolkit.hbase.tasks.snapshot;

import com.anttoolkit.hbase.tasks.GenericHBaseTask;
import org.apache.tools.ant.*;

public class DeleteSnapshotTask extends GenericHBaseTask
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
			getHBaseAdmin().deleteSnapshot(snapshot);
		}
		catch (Exception e)
		{
			throw new BuildException("Failed to delete snapshot " + snapshot);
		}
	}

	@Override
	protected void hadoopValidate()
	{
		if (snapshot == null || snapshot.trim().isEmpty())
		{
			throw new BuildException("Snapshot to delete should be specified");
		}
	}
}
