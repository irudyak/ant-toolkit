package com.anttoolkit.hbase.tasks.snapshot;

import java.io.*;
import java.util.*;

import org.apache.hadoop.hbase.protobuf.generated.HBaseProtos.SnapshotDescription;
import org.apache.tools.ant.*;

import com.anttoolkit.hbase.tasks.*;

public class CheckSnapshotExistsTask extends GenericHBaseTask
{
	private String snapshot;
	private String property;

	public void setSnapshot(String snapshot)
	{
		this.snapshot = snapshot;
	}

	public void setProperty(String property)
	{
		this.property = property;
	}

	@Override
	protected void doHadoopWork() throws BuildException
	{
		List<SnapshotDescription> snapshots;

		try
		{
			snapshots = getHBaseAdmin().listSnapshots();
		}
		catch (IOException e)
		{
			throw new BuildException("Failed to get list of available snapshots");
		}

		for (SnapshotDescription snapshotDescr : snapshots)
		{
			if (snapshotDescr.getName().equals(snapshot))
			{
				setPropertyThreadSafe(property, Boolean.TRUE.toString());
				return;
			}
		}

		setPropertyThreadSafe(property, Boolean.FALSE.toString());
	}

	@Override
	protected void hadoopValidate()
	{
		if (snapshot == null || snapshot.trim().isEmpty())
		{
			throw new BuildException("Snapshot name should be specified");
		}

		if (property == null || property.trim().isEmpty())
		{
			throw new BuildException("Property should be specified");
		}
	}
}
