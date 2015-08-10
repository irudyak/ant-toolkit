package com.anttoolkit.hbase.tasks;

import java.text.*;
import java.util.*;

import com.anttoolkit.general.common.SystemHelper;
import org.apache.hadoop.hbase.*;
import org.apache.hadoop.hbase.master.*;
import org.apache.hadoop.hbase.util.Bytes;
import org.apache.tools.ant.*;

import com.anttoolkit.general.tasks.collections.array.util.*;

public class GetClusterInfoTask extends GenericHBaseTask
{
	private static final String TITLE = "ClusterID: {0}, HBase version: {1}, Master: {2}, {3}, {4} live servers, {5} dead, {6} backup masters, {7} regions, {8} regions in transition, {9,number} avg load, {10} requests";

	private String property;
	private String avgLoadProp;
	private String backupMastersArray;
	private String balancerOnProp;
	private String clusterIdProp;
	private String deadServersArray;
	private String masterProp;
	private String hbaseVersionProp;
	private String masterCoprocessorsArray;
	private String regionsInTransitionArray;
	private String regionsCountProp;
	private String requestsCountProp;
	private String liveServersArray;
	private String regionsArray;

	public void setProperty(String property)
	{
		this.property = property;
	}

	public void setAvgLoadProperty(String property)
	{
		this.avgLoadProp = property;
	}

	public void setBackupMastersArray(String array)
	{
		backupMastersArray = array;
	}

	public void setBalancerOnProp(String property)
	{
		balancerOnProp = property;
	}

	public void setClusterIdProperty(String property)
	{
		clusterIdProp = property;
	}

	public void setDeadServersArray(String array)
	{
		deadServersArray = array;
	}

	public void setMasterProperty(String property)
	{
		masterProp = property;
	}

	public void setHbaseVersionProperty(String property)
	{
		hbaseVersionProp = property;
	}

	public void setMasterCoprocessorsArray(String array)
	{
		masterCoprocessorsArray = array;
	}

	public void setRegionsInTransitionArray(String array)
	{
		regionsInTransitionArray = array;
	}

	public void setRegionsCountProperty(String property)
	{
		regionsCountProp = property;
	}

	public void setRequestsCountProperty(String property)
	{
		requestsCountProp = property;
	}

	public void setLiveServersArray(String array)
	{
		liveServersArray = array;
	}

	public void setRegionsArray(String array)
	{
		regionsArray = array;
	}

	@Override
	protected void doHadoopWork() throws BuildException
	{
		ClusterStatus status = getClusterStatus();

		if (avgLoadProp != null)
		{
			setPropertyThreadSafe(avgLoadProp, Double.toString(status.getAverageLoad()));
		}

		if (backupMastersArray != null)
		{
			Collection<ServerName> servers = status.getBackupMasters();
			for (ServerName server : servers)
			{
				ArrayManager.add(backupMastersArray, server.getServerName());
			}
		}

		if (balancerOnProp != null)
		{
			setPropertyThreadSafe(balancerOnProp, status.getBalancerOn().toString());
		}

		if (clusterIdProp != null)
		{
			setPropertyThreadSafe(clusterIdProp, status.getClusterId());
		}

		if (deadServersArray != null)
		{
			Collection<ServerName> servers = status.getDeadServerNames();
			for (ServerName server : servers)
			{
				ArrayManager.add(deadServersArray, server.getServerName());
			}
		}

		if (masterProp != null)
		{
			setPropertyThreadSafe(masterProp, status.getMaster().getServerName());
		}

		if (hbaseVersionProp != null)
		{
			setPropertyThreadSafe(hbaseVersionProp, status.getHBaseVersion());
		}

		if (masterCoprocessorsArray != null)
		{
			String[] coprocessors = status.getMasterCoprocessors();
			for (String coprocessor : coprocessors)
			{
				ArrayManager.add(masterCoprocessorsArray, coprocessor);
			}
		}

		if (regionsInTransitionArray != null)
		{
			Collection<RegionState> states = status.getRegionsInTransition().values();
			for (RegionState state : states)
			{
				ArrayManager.add(regionsInTransitionArray, state.getRegion().getRegionNameAsString());
			}
		}

		if (regionsCountProp != null)
		{
			setPropertyThreadSafe(regionsCountProp, Integer.toString(status.getRegionsCount()));
		}

		if (requestsCountProp != null)
		{
			setPropertyThreadSafe(requestsCountProp, Integer.toString(status.getRequestsCount()));
		}

		if (liveServersArray != null)
		{
			Collection<ServerName> servers = status.getServers();
			for (ServerName server : servers)
			{
				ArrayManager.add(liveServersArray, server.getServerName());
			}
		}

		if (regionsArray != null)
		{
			fillRegionsArray(status);
		}

		if (property != null)
		{
			setPropertyThreadSafe(property, getStatusReport(status));
		}
	}

	@Override
	protected void hadoopValidate()
	{
		if (backupMastersArray != null && !ArrayManager.exists(backupMastersArray))
		{
			throw new BuildException("Array " + backupMastersArray + " doesn't exist");
		}

		if (deadServersArray != null && !ArrayManager.exists(deadServersArray))
		{
			throw new BuildException("Array " + deadServersArray + " doesn't exist");
		}

		if (masterCoprocessorsArray != null && !ArrayManager.exists(masterCoprocessorsArray))
		{
			throw new BuildException("Array " + masterCoprocessorsArray + " doesn't exist");
		}

		if (regionsInTransitionArray != null && !ArrayManager.exists(regionsInTransitionArray))
		{
			throw new BuildException("Array " + regionsInTransitionArray + " doesn't exist");
		}

		if (liveServersArray != null && !ArrayManager.exists(liveServersArray))
		{
			throw new BuildException("Array " + liveServersArray + " doesn't exist");
		}

		if (regionsArray != null && !ArrayManager.exists(regionsArray))
		{
			throw new BuildException("Array " + regionsArray + " doesn't exist");
		}
	}

	private String getStatusReport(ClusterStatus status)
	{
		String title = MessageFormat.format(TITLE, status.getClusterId(), status.getHBaseVersion(),
				status.getMaster().getServerName(), status.getBalancerOn() ? "BALANCER_ON" : "BALANCER_OFF",
				status.getServers().size(), status.getDeadServerNames().size(), status.getBackupMasters().size(),
				status.getRegionsCount(), status.getRegionsInTransition().size(), status.getAverageLoad(),
				status.getRequestsCount());

		StringBuilder builder = new StringBuilder();
		builder.append(title);

		builder.append(SystemHelper.lineSeparator).append(getRegionsInTransitionInfo(status));
		builder.append(SystemHelper.lineSeparator).append(getMasterCoprocessorsInfo(status));
		builder.append(SystemHelper.lineSeparator).append(getServersInfo(status, status.getServers(), "Live servers"));
		builder.append(SystemHelper.lineSeparator).append(getDeadServersInfo(status));
		builder.append(SystemHelper.lineSeparator).append(getServersInfo(status, status.getBackupMasters(), "Backup masters"));

		return builder.toString();
	}

	private String getRegionsInTransitionInfo(ClusterStatus status)
	{
		Collection<RegionState> states = status.getRegionsInTransition().values();
		if (states.isEmpty())
		{
			return "0 Regions in transition";
		}

		StringBuilder builder = new StringBuilder(states.size() + " regions in transition:");

		for (RegionState state : states)
		{
			builder.append(SystemHelper.lineSeparator).append("\t").append(state.toDescriptiveString());
		}

		return builder.toString();
	}

	private String getMasterCoprocessorsInfo(ClusterStatus status)
	{
		String[] coprocessors = status.getMasterCoprocessors();
		if (coprocessors == null || coprocessors.length == 0)
		{
			return "0 Master coprocessors";
		}

		StringBuilder builder = new StringBuilder(coprocessors.length + " Master coprocessors:");

		for (String coprocessor : coprocessors)
		{
			builder.append(SystemHelper.lineSeparator).append("\t").append(coprocessor);
		}

		return builder.toString();
	}

	private String getDeadServersInfo(ClusterStatus status)
	{
		Collection<ServerName> servers = status.getDeadServerNames();

		if (servers == null || servers.size() == 0)
		{
			return "0 Dead servers";
		}

		StringBuilder builder = new StringBuilder(servers.size() + " Dead servers:");

		for (ServerName server : servers)
		{
			String serverSummary = SystemHelper.lineSeparator + "\t" + server.getHostname() + ":" + server.getPort() + " " + server.getStartcode();
			builder.append(serverSummary);
		}

		return builder.toString();
	}

	private String getServersInfo(ClusterStatus status, Collection<ServerName> servers, String title)
	{
		if (servers == null || servers.size() == 0)
		{
			return "0 " + title;
		}

		StringBuilder builder = new StringBuilder(servers.size() + " " + title + ":");

		for (ServerName server : servers)
		{
			String serverSummary = SystemHelper.lineSeparator + "\tSERVER: " + server.getHostname() + ":" + server.getPort() + " " + server.getStartcode();
			String serverLoad = SystemHelper.lineSeparator + "\t\tSERVER_LOAD: " + status.getLoad(server).toString();

			builder.append(serverSummary).append(serverLoad);

			Collection<RegionLoad> regions = status.getLoad(server).getRegionsLoad().values();
			for (RegionLoad region : regions)
			{
				String regionSummary = SystemHelper.lineSeparator + "\t\t\tREGION: " + region.getNameAsString();
				String regionLoad = SystemHelper.lineSeparator + "\t\t\t\tREGION_LOAD: " + region.toString();

				builder.append(regionSummary).append(regionLoad);
			}
		}

		return builder.toString();
	}

	private void fillRegionsArray(ClusterStatus status)
	{
		List<String> liveRegions = getServerRegions(status, status.getServers(), true);
		List<String> deadRegions = getServerRegions(status, status.getDeadServerNames(), false);
		List<String> backupRegions = getServerRegions(status, status.getBackupMasters(), false);

		for (String region : deadRegions)
		{
			if (!liveRegions.contains(region))
			{
				liveRegions.add(region);
			}
		}

		for (String region : backupRegions)
		{
			if (!liveRegions.contains(region))
			{
				liveRegions.add(region);
			}
		}

		for (String region : liveRegions)
		{
			ArrayManager.add(regionsArray, region);
		}

		liveRegions.clear();
		deadRegions.clear();
		backupRegions.clear();
	}

	private List<String> getServerRegions(ClusterStatus status, Collection<ServerName> servers, boolean isLiveServers)
	{
		List<String> regions = new LinkedList<String>();

		if (servers == null || servers.isEmpty())
		{
			return regions;
		}

		for (ServerName server : servers)
		{
			try
			{
				ServerLoad serverLoad = status.getLoad(server);
				Set<byte[]> serverRegions = serverLoad.getRegionsLoad().keySet();

				for (byte[] region : serverRegions)
				{
					String regionName = Bytes.toString(region);
					if (!regions.contains(regionName))
					{
						regions.add(regionName);
					}
				}
			}
			catch (Throwable e)
			{
				if (isLiveServers)
				{
					if (e instanceof RuntimeException)
					{
						throw (RuntimeException)e;
					}
					else
					{
						throw new BuildException("Exception occured while trying to get load of HBase server " + server.getServerName(), e);
					}
				}
			}
		}

		return regions;
	}
}
