package com.anttoolkit.hbase.common;

import java.io.*;
import java.util.*;

import org.apache.hadoop.conf.*;
import org.apache.hadoop.hbase.*;
import org.apache.hadoop.hbase.client.*;
import org.apache.tools.ant.*;

public class HBaseResourcesProvider
{
	private Configuration conf;
	private HConnection conn;
	private HBaseAdmin admin;
	private Map<String, HTableInterface> tables = new HashMap<String, HTableInterface>();

	public HBaseResourcesProvider(Configuration conf)
	{
		if (conf == null)
		{
			throw new IllegalArgumentException("Can't use null Configuration for HBaseResourcesProvider");
		}

		this.conf = conf;
	}

	public HBaseAdmin getAdmin()
	{
		if (admin != null)
		{
			return admin;
		}

		try
		{
			return admin = new HBaseAdmin(conf);
		}
		catch (MasterNotRunningException e)
		{
			throw new BuildException("HBase master server is not running", e);
		}
		catch (ZooKeeperConnectionException e)
		{
			throw new BuildException("Failed to connect to ZooKeeper quorum", e);
		}
		catch (IOException e)
		{
			throw new BuildException("Failed to connect to HBase instance", e);
		}
	}

	public HTableInterface getTable(String tableName)
	{
		if (tableName == null)
		{
			return null;
		}

		HTableInterface table = tables.get(tableName);
		if (table != null)
		{
			return table;
		}

		try
		{
			table = getConnection().getTable(tableName);
		}
		catch (IOException e)
		{
			throw new BuildException("Failed to get HBase table " + tableName, e);
		}

		tables.put(tableName, table);

		return table;
	}

	public HConnection getConnection()
	{
		if (conn != null)
		{
			return conn;
		}

		try
		{
			return conn = HConnectionManager.createConnection(conf);
		}
		catch (IOException e)
		{
			throw new BuildException("Failed to establish connection to HBase instance", e);
		}
	}

	public void closeTable(HTableInterface table)
	{
		if (table == null)
		{
			return;
		}

		if (tables.containsKey(table.getName().getNamespaceAsString()) &&
			table.equals(tables.get(table.getName().getNameAsString())))
		{
			tables.remove(table.getName().getNameAsString());
		}

		try
		{
			table.close();
		}
		catch (IOException e)
		{
		}
	}

	public void closeAdmin(HBaseAdmin admin)
	{
		if (admin == null)
		{
			return;
		}

		if (admin.equals(this.admin))
		{
			this.admin = null;
		}

		try
		{
			admin.close();
		}
		catch (IOException e)
		{
		}
	}

	public void closeConnection(HConnection conn)
	{
		if (conn == null)
		{
			return;
		}

		if (conn.equals(this.conn))
		{
			this.conn = null;
			releaseTables();
		}

		try
		{
			conn.close();
		}
		catch (IOException e)
		{
		}
	}

	public void releaseResources()
	{
		releaseTables();
		releaseConnection();
		releaseAdmin();
	}

	protected void finalize()
			throws Throwable
	{
		releaseResources();
		super.finalize();
	}

	private void releaseAdmin()
	{
		if (admin == null)
		{
			return;
		}

		try
		{
			admin.close();
		}
		catch (IOException e)
		{
		}
		finally
		{
			admin = null;
		}
	}

	private void releaseConnection()
	{
		if (conn == null)
		{
			return;
		}

		try
		{
			conn.close();
		}
		catch (IOException e)
		{
		}
		finally
		{
			conn = null;
		}
	}

	private void releaseTables()
	{
		if (tables.isEmpty())
		{
			return;
		}

		for (HTableInterface table : tables.values())
		{
			try
			{
				table.close();
			}
			catch (IOException e)
			{
			}
		}

		tables.clear();
	}
}
