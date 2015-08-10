package com.anttoolkit.hbase.tasks.table;

import java.io.*;
import java.util.*;

import org.apache.hadoop.hbase.*;
import org.apache.hadoop.hbase.client.*;
import org.apache.hadoop.hbase.util.*;
import org.apache.tools.ant.*;

import com.anttoolkit.hbase.tasks.*;
import com.anttoolkit.hbase.tasks.table.util.*;
import com.anttoolkit.hbase.tasks.table.util.Coprocessor;

public class AlterTableTask extends GenericHBaseTask
{
	private static final long SLEEP_TIMEOUT = 10000;
	private static final long ONE_ROUND_WAIT = 3000;

	private String table;
	private Durability durability;
	private Long maxFileSize;
	private Long memstoreFlushSize;
	private Boolean compaction;
	private boolean asynch = true;

	private List<ColumnFamily> columnFamilies = new LinkedList<ColumnFamily>();
	private List<Coprocessor> coprocessors = new LinkedList<com.anttoolkit.hbase.tasks.table.util.Coprocessor>();
	private List<KeyValuePair> metadata = new LinkedList<KeyValuePair>();
	private List<KeyValuePair> configs = new LinkedList<KeyValuePair>();

	private List<String> columnFamiliesToRemove = new LinkedList<String>();
	private List<String> coprocessorsToRemove = new LinkedList<String>();
	private List<String> metadataToRemove = new LinkedList<String>();
	private List<String> configsToRemove = new LinkedList<String>();

	public void setAsynch(boolean asynch)
	{
		this.asynch = asynch;
	}

	public void setTable(String table)
	{
		this.table = table;
	}

	public void setDurability(String durability)
	{
		this.durability = Durability.valueOf(durability.toUpperCase());
	}

	public void setMaxFileSize(long maxFileSize)
	{
		this.maxFileSize = maxFileSize;
	}

	public void setMemStoreFlushSize(long memstoreFlushSize)
	{
		this.memstoreFlushSize = memstoreFlushSize;
	}

	public void setCompaction(boolean isEnabled)
	{
		compaction = isEnabled;
	}

	public void addConfiguredColumnFamily(ColumnFamily family)
	{
		if (family == null)
		{
			throw new IllegalArgumentException("Column family can't be null");
		}

		family.validate();
		columnFamilies.add(family);
	}

	public void addConfiguredRemoveColumnFamily(ColumnFamily family)
	{
		if (family == null)
		{
			throw new IllegalArgumentException("Column family can't be null");
		}

		family.validate();
		columnFamiliesToRemove.add(family.getName());
	}

	public void addConfiguredCoprocessor(com.anttoolkit.hbase.tasks.table.util.Coprocessor coprocessor)
	{
		if (coprocessor == null)
		{
			throw new IllegalArgumentException("Coprocessor can't be null");
		}

		coprocessor.validate();
		coprocessors.add(coprocessor);
	}

	public void addConfiguredRemoveCoprocessor(com.anttoolkit.hbase.tasks.table.util.Coprocessor coprocessor)
	{
		if (coprocessor == null)
		{
			throw new IllegalArgumentException("Coprocessor can't be null");
		}

		coprocessor.validate();
		coprocessorsToRemove.add(coprocessor.getClassName());
	}

	public void addConfiguredMetadata(KeyValuePair pair)
	{
		if (pair == null)
		{
			throw new IllegalArgumentException("Metadata can't be null");
		}

		pair.validate();
		metadata.add(pair);
	}

	public void addConfiguredRemoveMetadata(KeyValuePair pair)
	{
		if (pair == null)
		{
			throw new IllegalArgumentException("Metadata can't be null");
		}

		pair.validate();
		metadataToRemove.add(pair.getKey());
	}

	public void addConfiguredConfiguration(KeyValuePair pair)
	{
		if (pair == null)
		{
			throw new IllegalArgumentException("Configuration can't be null");
		}

		pair.validate();
		configs.add(pair);
	}

	public void addConfiguredRemoveConfiguration(KeyValuePair pair)
	{
		if (pair == null)
		{
			throw new IllegalArgumentException("Configuration can't be null");
		}

		pair.validate();
		configsToRemove.add(pair.getKey());
	}

	@Override
	protected void doHadoopWork() throws BuildException
	{
		HTableDescriptor htable;

		try
		{
			htable = getHBaseAdmin().getTableDescriptor(TableName.valueOf(table));
		}
		catch (Exception e)
		{
			throw new BuildException("Failed to get descriptor for HBase table " + table, e);
		}

		boolean settingsChanged = changeTableSettings(htable);
		boolean metadataChanged = alterMetadata(htable);
		boolean coprocessorsChanged = alterCoprocessors(htable);
		boolean configChanged = alterConfiguration(htable);

		if (settingsChanged || metadataChanged || coprocessorsChanged || configChanged)
		{
			try
			{
				getHBaseAdmin().modifyTable(table, htable);
			}
			catch (IOException e)
			{
				throw new BuildException("Failed to modify " + table + " table settings", e);
			}
		}

		alterColumnFamilies(htable);

		try
		{
			Thread.sleep(ONE_ROUND_WAIT);
		}
		catch (InterruptedException e) { }

		if (this.checkTableAlterationCompleted(table))
		{
			log("HBase table " + table + " was altered");
			return;
		}

		if (asynch)
		{
			log("HBase table " + table + " alteration was started and still in progress");
			return;
		}

		log("HBase table " + table + " alteration was started and still in progress");
		log("Waiting for HBase table " + table + " alteration to complete...");

		while (!this.checkTableAlterationCompleted(table))
		{
			try
			{
				Thread.sleep(SLEEP_TIMEOUT);
			}
			catch (InterruptedException e)
			{
				throw new BuildException("HBase table " + table + " alteration wait was interrupted", e);
			}
		}

		log("HBase table " + table + " alteration completed");
	}

	@Override
	protected void hadoopValidate()
	{
		if (table == null || table.trim().isEmpty())
		{
			throw new BuildException("Table name should be specified");
		}

		try
		{
			if (!getHBaseAdmin().tableExists(table) )
			{
				throw new BuildException("HBase table " + table + " doesn't exist");
			}
		}
		catch (IOException e)
		{
			throw new BuildException("Failed to check if HBase table " + table + " already exists", e);
		}
	}

	private boolean changeTableSettings(HTableDescriptor htable)
	{
		if (durability != null)
		{
			htable.setDurability(durability);
		}

		if (maxFileSize != null)
		{
			htable.setMaxFileSize(maxFileSize);
		}

		if (memstoreFlushSize != null)
		{
			htable.setMemStoreFlushSize(memstoreFlushSize);
		}

		if (compaction != null)
		{
			htable.setCompactionEnabled(compaction);
		}

		return durability != null || maxFileSize != null || memstoreFlushSize != null || compaction != null;
	}

	private boolean alterCoprocessors(HTableDescriptor htable)
	{
		for (String coprocessorClass : coprocessorsToRemove)
		{
			htable.removeCoprocessor(coprocessorClass);
		}

		for (com.anttoolkit.hbase.tasks.table.util.Coprocessor coprocessor : coprocessors)
		{
			coprocessor.addCorprocessor(htable);
		}

		return !coprocessorsToRemove.isEmpty() || !coprocessors.isEmpty();
	}

	private boolean alterMetadata(HTableDescriptor htable)
	{
		for (String key : metadataToRemove)
		{
			htable.remove(key);
		}

		for (KeyValuePair pair : metadata)
		{
			htable.setValue(pair.getKey(), pair.getValue());
		}

		return !metadataToRemove.isEmpty() || !metadata.isEmpty();
	}

	private boolean alterConfiguration(HTableDescriptor htable)
	{
		for (String key : configsToRemove)
		{
			htable.removeConfiguration(key);
		}

		for (KeyValuePair pair : configs)
		{
			htable.setConfiguration(pair.getKey(), pair.getValue());
		}

		return !configsToRemove.isEmpty() || !configs.isEmpty();
	}

	private void alterColumnFamilies(HTableDescriptor htable)
	{
		for (String columnFamily : columnFamiliesToRemove)
		{
			try
			{
				getHBaseAdmin().deleteColumn(table, columnFamily);
			}
			catch (IOException e)
			{
				throw new BuildException("Failed to delete " + columnFamily + " column family of HBase table " + table, e);
			}
		}

		for (ColumnFamily columnFamily : columnFamilies)
		{
			boolean exists = htable.getFamiliesKeys().contains(Bytes.toBytes(columnFamily.getName()));

			if (exists)
			{
				try
				{
					getHBaseAdmin().modifyColumn(table, columnFamily.getUpdatedColumnDescriptor(htable));
				}
				catch (IOException e)
				{
					throw new BuildException("Failed to modify " + columnFamily.getName() + " column family of table " + table, e);
				}
			}
			else
			{
				try
				{
					getHBaseAdmin().addColumn(table, columnFamily.createColumnDescriptor());
				}
				catch (IOException e)
				{
					throw new BuildException("Failed to add " + columnFamily.getName() + " column family to table " + table, e);
				}
			}
		}
	}
}
