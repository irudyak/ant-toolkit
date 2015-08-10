package com.anttoolkit.hbase.tasks.snapshot;

import java.io.*;
import java.util.*;

import com.anttoolkit.general.refs.*;
import org.apache.hadoop.hbase.protobuf.generated.HBaseProtos.SnapshotDescription;
import org.apache.tools.ant.*;

import com.anttoolkit.hbase.tasks.*;

public class GetSnapshotInfoTask extends GenericHBaseTask
{
	private String snapshot;
	private String reference;

	public void setReference(String ref)
	{
		reference = ref;
	}

	public void setSnapshot(String snapshot)
	{
		this.snapshot = snapshot;
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
				this.setReference(reference, snapshotDescr);
				return;
			}
		}

		throw new BuildException("There is no snapshot " + snapshot + " among available snapshots");
	}

	@Override
	protected void hadoopValidate()
	{
		if (reference == null || reference.trim().isEmpty())
		{
			throw new BuildException("Reference name should be specified");
		}

		if (snapshot == null || snapshot.trim().isEmpty())
		{
			throw new BuildException("Snapshot name should be specified");
		}
	}
}
