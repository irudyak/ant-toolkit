package com.anttoolkit.hbase.tasks.snapshot;

import com.anttoolkit.hbase.tasks.GenericHBaseTask;
import org.apache.hadoop.hbase.protobuf.generated.HBaseProtos.SnapshotDescription;
import org.apache.tools.ant.*;

public class CreateSnapshotTask extends GenericHBaseTask
{
	private String table;
	private String snapshot;
	private SnapshotDescription.Type type = SnapshotDescription.Type.FLUSH;

	public void setTable(String table)
	{
		this.table = table;
	}

	public void setSnapshot(String snapshot)
	{
		this.snapshot = snapshot;
	}

	public void setType(String type)
	{
		this.type = SnapshotDescription.Type.valueOf(type);
	}

	@Override
	protected void doHadoopWork() throws BuildException
	{
		try
		{
			getHBaseAdmin().snapshot(snapshot, table, type);
		}
		catch (Exception e)
		{
			throw new BuildException("Failed to create snapshot " + snapshot + " for table " + table);
		}
	}

	protected void hadoopValidate()
	{
		if (table == null || table.trim().isEmpty())
		{
			throw new BuildException("Table for snapshot should be specified");
		}

		if (snapshot == null || snapshot.trim().isEmpty())
		{
			throw new BuildException("Snapshot name for table " + table + " should be specified");
		}
	}
}
