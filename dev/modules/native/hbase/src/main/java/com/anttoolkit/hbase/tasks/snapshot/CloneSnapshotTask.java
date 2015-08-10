package com.anttoolkit.hbase.tasks.snapshot;

import com.anttoolkit.hbase.tasks.GenericHBaseTask;
import org.apache.tools.ant.*;

public class CloneSnapshotTask extends GenericHBaseTask
{
	private String table;
	private String snapshot;

	public void setTable(String table)
	{
		this.table = table;
	}

	public void setSnapshot(String snapshot)
	{
		this.snapshot = snapshot;
	}

	@Override
	protected void doHadoopWork() throws BuildException
	{
		try
		{
			getHBaseAdmin().cloneSnapshot(snapshot, table);
		}
		catch (Exception e)
		{
			throw new BuildException("Failed to clone snapshot " + snapshot + " to create " + table + " table");
		}
	}

	protected void hadoopValidate()
	{
		if (table == null || table.trim().isEmpty())
		{
			throw new BuildException("Table which will be created from snapshot should be specified");
		}

		if (snapshot == null || snapshot.trim().isEmpty())
		{
			throw new BuildException("Snapshot name for table " + table + " should be specified");
		}
	}
}
