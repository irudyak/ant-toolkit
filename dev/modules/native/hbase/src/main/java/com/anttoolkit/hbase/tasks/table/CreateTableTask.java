package com.anttoolkit.hbase.tasks.table;

import java.io.*;
import java.util.*;

import org.apache.tools.ant.*;

import org.apache.hadoop.hbase.*;
import org.apache.hadoop.hbase.client.*;

import com.anttoolkit.hbase.tasks.*;
import com.anttoolkit.hbase.tasks.table.util.*;
import com.anttoolkit.hbase.tasks.table.util.Coprocessor;

public class CreateTableTask extends GenericHBaseTask
{
	private String table;
	private Durability durability;
	private Long maxFileSize;
	private Long memstoreFlushSize;
	private Boolean compaction;
	private boolean ignoreIfExist = false;

	private List<ColumnFamily> columnFamilies = new LinkedList<ColumnFamily>();
	private List<Coprocessor> coprocessors = new LinkedList<com.anttoolkit.hbase.tasks.table.util.Coprocessor>();
	private List<KeyValuePair> metadata = new LinkedList<KeyValuePair>();
	private List<KeyValuePair> configs = new LinkedList<KeyValuePair>();

	public void setIgnoreIfExist(boolean ignore)
	{
		ignoreIfExist = ignore;
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

	public void addConfiguredCoprocessor(com.anttoolkit.hbase.tasks.table.util.Coprocessor coprocessor)
	{
		if (coprocessor == null)
		{
			throw new IllegalArgumentException("Coprocessor can't be null");
		}

		coprocessor.validate();
		coprocessors.add(coprocessor);
	}

	public void addConfiguredMetadata(KeyValuePair value)
	{
		if (value == null)
		{
			throw new IllegalArgumentException("Metadata can't be null");
		}

		value.validate();
		metadata.add(value);
	}

	public void addConfiguredConfiguration(KeyValuePair config)
	{
		if (config == null)
		{
			throw new IllegalArgumentException("Configuration can't be null");
		}

		config.validate();
		configs.add(config);
	}

	@Override
	protected void doHadoopWork() throws BuildException
	{
		try
		{
			if (ignoreIfExist && getHBaseAdmin().tableExists(table) )
			{
				log("HBase table " + table + " already exists");
				return;
			}
		}
		catch (IOException e)
		{
			throw new BuildException("Failed to check if HBase table " + table + " already exists", e);
		}

		HTableDescriptor htable = new HTableDescriptor(TableName.valueOf(table));

		setTableSettings(htable);
		addCoprocessors(htable);
		addMetadata(htable);
		addConfiguration(htable);
		addColumnFamilies(htable);

		try
		{
			getHBaseAdmin().createTable(htable);
		}
		catch (IOException e)
		{
			throw new BuildException("Failed to create HBase table " + table, e);
		}

		log("HBase table " + table + " was created");
	}

	@Override
	protected void hadoopValidate()
	{
		if (table == null || table.trim().isEmpty())
		{
			throw new BuildException("Table name should be specified");
		}

		if (columnFamilies.isEmpty())
		{
			throw new BuildException("No column families specified for HBase table " + table);
		}
	}

	private void setTableSettings(HTableDescriptor htable)
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
	}

	private void addCoprocessors(HTableDescriptor htable)
	{
		for (com.anttoolkit.hbase.tasks.table.util.Coprocessor coprocessor : coprocessors)
		{
			coprocessor.addCorprocessor(htable);
		}
	}

	private void addMetadata(HTableDescriptor htable)
	{
		for (KeyValuePair pair : metadata)
		{
			htable.setValue(pair.getKey(), pair.getValue());
		}
	}

	private void addConfiguration(HTableDescriptor htable)
	{
		for (KeyValuePair pair : configs)
		{
			htable.setConfiguration(pair.getKey(), pair.getValue());
		}
	}

	private void addColumnFamilies(HTableDescriptor htable)
	{
		for (ColumnFamily columnFamily : columnFamilies)
		{
			htable.addFamily(columnFamily.createColumnDescriptor());
		}
	}

}
