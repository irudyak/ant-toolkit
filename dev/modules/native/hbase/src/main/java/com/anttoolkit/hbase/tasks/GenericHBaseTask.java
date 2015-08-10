package com.anttoolkit.hbase.tasks;

import java.io.*;
import java.util.*;

import com.anttoolkit.hbase.common.HBaseResourcesManager;
import com.anttoolkit.hbase.common.HBaseResourcesProvider;
import org.apache.hadoop.conf.*;
import org.apache.hadoop.hbase.*;
import org.apache.hadoop.hbase.client.*;
import org.apache.hadoop.hbase.util.*;
import org.apache.tools.ant.*;

import com.anttoolkit.hadoop.tasks.hadoop.*;

public abstract class GenericHBaseTask extends GenericHadoopTask
{
	private static final String ZOOKEEPER_QUORUM_PROP = "hbase.zookeeper.quorum";

	private String zookeeperQuorum;
	private String prevZookeeperQuorum;
	private HBaseResourcesProvider provider;

	public void setZookeeperQuorum(String quorum)
	{
		zookeeperQuorum = quorum;
	}

	@Override
	protected void setHadoopContext()
	{
		prevZookeeperQuorum = System.getProperties().containsKey(ZOOKEEPER_QUORUM_PROP) ?
				System.getProperties().getProperty(ZOOKEEPER_QUORUM_PROP) : null;

		if (zookeeperQuorum != null)
		{
			System.getProperties().setProperty(ZOOKEEPER_QUORUM_PROP, zookeeperQuorum);
		}

		super.setHadoopContext();
	}

	@Override
	protected void releaseHadoopContext()
	{
		if (prevZookeeperQuorum != null)
		{
			System.getProperties().setProperty(ZOOKEEPER_QUORUM_PROP, prevZookeeperQuorum);
		}

		super.releaseHadoopContext();
	}

	@Override
	protected Configuration getConfiguration()
	{
		Configuration conf = HBaseConfiguration.create(super.getConfiguration());
		if (zookeeperQuorum != null)
		{
			conf.set(ZOOKEEPER_QUORUM_PROP, zookeeperQuorum);
		}

		return conf;
	}

	@Override
	protected void releaseHadoopResources()
	{
		if (provider != null && !provider.equals(HBaseResourcesManager.getProvider()))
		{
			provider.releaseResources();
			provider = null;
		}

		super.releaseHadoopResources();
	}

	protected final HBaseAdmin getHBaseAdmin()
	{
		return getProvider().getAdmin();
	}

	protected final HConnection getConnection()
	{
		return getProvider().getConnection();
	}

	protected final HTableInterface getTable(String tableName)
	{
		return getProvider().getTable(tableName);
	}

	protected final ClusterStatus getClusterStatus()
	{
		try
		{
			return getHBaseAdmin().getClusterStatus();
		}
		catch (IOException e)
		{
			throw new BuildException("Failed to get HBase cluster status", e);
		}
	}

	protected final ServerName getServer(String serverName)
	{
		ClusterStatus status;

		try
		{
			status = getHBaseAdmin().getClusterStatus();
		}
		catch (IOException e)
		{
			throw new BuildException("Failed to get cluster status", e);
		}

		ServerName server = getServer(status.getServers(), serverName);
		if (server != null)
		{
			return server;
		}

		server = getServer(status.getDeadServerNames(), serverName);
		if (server != null)
		{
			return server;
		}

		server = getServer(status.getBackupMasters(), serverName);
		if (server != null)
		{
			return server;
		}

		throw new BuildException("Failed to find HBase server with name: " + serverName);
	}

	protected final void enableTable(String tableName)
	{
		try
		{
			if (!getHBaseAdmin().isTableEnabled(tableName))
			{
				getHBaseAdmin().enableTable(tableName);
			}
		}
		catch (IOException e)
		{
			throw new BuildException("Failed to enable HBase table " + tableName, e);
		}
	}

	protected final void dropTable(String tableName)
	{
		try
		{
			boolean disabled = getHBaseAdmin().isTableDisabled(tableName);

			if (!disabled)
			{
				getHBaseAdmin().disableTable(tableName);
			}

			getHBaseAdmin().deleteTable(tableName);
		}
		catch (IOException e)
		{
			throw new BuildException("Failed to delete HBase table " + tableName);
		}
	}

	protected final void disableTable(String tableName)
	{
		try
		{
			if (!getHBaseAdmin().isTableDisabled(tableName))
			{
				getHBaseAdmin().disableTable(tableName);
			}
		}
		catch (IOException e)
		{
			throw new BuildException("Failed to enable HBase table " + tableName, e);
		}
	}

	protected final String getServerForRegion(String regionName)
	{
		ClusterStatus status = getClusterStatus();

		for (ServerName server : status.getServers())
		{
			RegionLoad regionLoad = getRegionLoad(status, server, regionName);
			if (regionLoad != null)
			{
				return server.getServerName();
			}
		}

		for (ServerName server : status.getDeadServerNames())
		{
			try
			{
				RegionLoad regionLoad = getRegionLoad(status, server, regionName);
				if (regionLoad != null)
				{
					return server.getServerName();
				}
			}
			catch (Throwable e) {}
		}

		for (ServerName server : status.getBackupMasters())
		{
			try
			{
				RegionLoad regionLoad = getRegionLoad(status, server, regionName);
				if (regionLoad != null)
				{
					return server.getServerName();
				}
			}
			catch (Throwable e) {}
		}

		return null;
	}

	protected final RegionLoad getRegionLoad(String regionName)
	{
		ClusterStatus status = getClusterStatus();

		for (ServerName server : status.getServers())
		{
			RegionLoad regionLoad = getRegionLoad(status, server, regionName);
			if (regionLoad != null)
			{
				return regionLoad;
			}
		}

		for (ServerName server : status.getDeadServerNames())
		{
			try
			{
				RegionLoad regionLoad = getRegionLoad(status, server, regionName);
				if (regionLoad != null)
				{
					return regionLoad;
				}
			}
			catch (Throwable e) {}
		}

		for (ServerName server : status.getBackupMasters())
		{
			try
			{
				RegionLoad regionLoad = getRegionLoad(status, server, regionName);
				if (regionLoad != null)
				{
					return regionLoad;
				}
			}
			catch (Throwable e) {}
		}

		return null;
	}

	protected final RegionLoad getRegionLoad(String serverName, String regionName)
	{
		return getRegionLoad(getClusterStatus(), ServerName.valueOf(serverName), regionName);
	}

	protected final boolean checkRegionExists(String regionName)
	{
		return getRegionLoad(regionName) != null;
	}

	protected final boolean checkTableAlterationCompleted(String tableName)
	{
		try
		{
			Pair<Integer, Integer> pair = getHBaseAdmin().getAlterStatus(TableName.valueOf(tableName));
			return pair.getFirst() == 0;
		}
		catch (IOException e)
		{
			throw new BuildException("Failed to get alter status for table " + tableName, e);
		}
	}

	private RegionLoad getRegionLoad(ClusterStatus status, ServerName serverName, String regionName)
	{
		ServerLoad serverLoad = status.getLoad(serverName);
		return getRegionLoad(serverLoad, regionName);
	}

	private RegionLoad getRegionLoad(ServerLoad serverLoad, String regionName)
	{
		Map<byte[], RegionLoad> regionsLoad = serverLoad.getRegionsLoad();

		for (byte[] region : regionsLoad.keySet())
		{
			String name = Bytes.toString(region);
			if (name.equals(regionName))
			{
				return regionsLoad.get(region);
			}
		}

		return null;
	}

	private ServerName getServer(Collection<ServerName> servers, String serverName)
	{
		if (servers == null)
		{
			return null;
		}

		for (ServerName server : servers)
		{
			if (server.getServerName().equals(serverName))
			{
				return server;
			}
		}

		return null;
	}

	private HBaseResourcesProvider getProvider()
	{
		if (provider != null)
		{
			return provider;
		}

		return provider = HBaseResourcesManager.getProvider() != null ?
				HBaseResourcesManager.getProvider() :
				new HBaseResourcesProvider(getConfiguration());
	}
}
