package com.anttoolkit.hbase.tasks.server;

import java.util.*;

import com.anttoolkit.hbase.tasks.GenericHBaseTask;
import org.apache.hadoop.hbase.*;
import org.apache.hadoop.hbase.util.*;
import org.apache.tools.ant.*;

import com.anttoolkit.general.tasks.collections.array.util.*;

public class GetServerRegionsTask extends GenericHBaseTask
{
	private String server;
	private String array;

	public void setServer(String name)
	{
		server = name;
	}

	public void setArray(String array)
	{
		this.array = array;
	}

	@Override
	protected void doHadoopWork() throws BuildException
	{
		ServerLoad serverLoad = getClusterStatus().getLoad(getServer(server));
		Set<byte[]> regions = serverLoad.getRegionsLoad().keySet();

		for (byte[] region : regions)
		{
			ArrayManager.add(array, Bytes.toString(region));
		}
	}

	@Override
	protected void hadoopValidate()
	{
		if (server == null || server.trim().isEmpty())
		{
			throw new BuildException("Server name where to look for HBase regions should be specified");
		}

		if (array == null || array.trim().isEmpty())
		{
			throw new BuildException("Array name to store HBase regions should be specified");
		}

		if (!ArrayManager.exists(array))
		{
			throw new BuildException("Array " + array + " doesn't exist");
		}
	}
}
