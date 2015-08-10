package com.anttoolkit.hbase.tasks.snapshot;

import java.io.*;
import java.util.*;

import com.anttoolkit.hbase.tasks.*;
import org.apache.hadoop.hbase.protobuf.generated.HBaseProtos.SnapshotDescription;
import org.apache.tools.ant.*;

import com.anttoolkit.general.tasks.collections.map.util.*;

public class GetSnapshotsTask extends GenericHBaseTask
{
	private String map;

	public void setMap(String map)
	{
		this.map = map;
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

		if (snapshots == null)
		{
			return;
		}

		for (SnapshotDescription snapshot : snapshots)
		{
			MapManager.put(map, snapshot.getName(), snapshot.getTable());
		}
	}

	@Override
	protected void hadoopValidate()
	{
		if (map == null || map.trim().isEmpty())
		{
			throw new BuildException("Map to store snapshot to table mapping should be specified");
		}

		if (MapManager.exists(map))
		{
			throw new BuildException("Specified map " + map + " doesn't exist");
		}
	}
}
