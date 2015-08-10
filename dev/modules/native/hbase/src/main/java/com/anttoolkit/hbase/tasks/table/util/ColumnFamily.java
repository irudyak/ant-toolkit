package com.anttoolkit.hbase.tasks.table.util;

import java.util.*;

import com.anttoolkit.hbase.tasks.KeyValuePair;
import org.apache.hadoop.hbase.*;
import org.apache.hadoop.hbase.io.compress.*;
import org.apache.hadoop.hbase.io.encoding.*;
import org.apache.hadoop.hbase.regionserver.*;
import org.apache.hadoop.hbase.util.*;
import org.apache.tools.ant.BuildException;

public class ColumnFamily
{
	private String name;
	private Integer maxVersions;
	private Integer minVersions;
	private Boolean keepDeletedCells;
	private Compression.Algorithm compression;
	private Boolean encodeOnDisk;
	private DataBlockEncoding dataBlockEncoding;
	private Boolean inMemory;
	private Boolean blockCache;
	private Integer blockSize;
	private Integer timeToLive;
	private String bloomFilter;
	private Integer scope;
	private Boolean cacheBloomsOnWrite;
	private Boolean cacheDataOnWrite;
	private Boolean cacheIndexesOnWrite;
	private Compression.Algorithm compactionCompression;
	private Boolean compressTags;
	private String encryptionType;
	private String encryptionKey;
	private Boolean evictBlocksOnClose;

	private List<KeyValuePair> configs = new LinkedList<KeyValuePair>();
	private List<KeyValuePair> metadata = new LinkedList<KeyValuePair>();

	private List<KeyValuePair> configsToRemove = new LinkedList<KeyValuePair>();
	private List<KeyValuePair> metadataToRemove = new LinkedList<KeyValuePair>();

	public void validate()
	{
		if (name == null)
		{
			throw new IllegalStateException("Column family name couldn't be null");
		}
	}

	public void setName(String name)
	{
		this.name = name;
	}

	public String getName()
	{
		return name;
	}

	public void setMaxVersions(int maxVersions)
	{
		this.maxVersions = maxVersions;
	}

	public void setMinVersions(int minVersions)
	{
		this.minVersions = minVersions;
	}

	public void setKeepDeletedCells(boolean keep)
	{
		keepDeletedCells = keep;
	}

	public void setCompression(String compression)
	{
		this.compression = Compression.Algorithm.valueOf(compression.toUpperCase());
	}

	public void setEncodeOnDisk(boolean encodeOnDisk)
	{
		this.encodeOnDisk = encodeOnDisk;
	}

	public void setDataBlockEncoding(String dataBlockEncoding)
	{
		this.dataBlockEncoding = DataBlockEncoding.valueOf(dataBlockEncoding.toUpperCase());
	}

	public void setInMemory(boolean inMemory)
	{
		this.inMemory = inMemory;
	}

	public void setBlockCache(boolean blockCache)
	{
		this.blockCache = blockCache;
	}

	public void setBlockSize(int blockSize)
	{
		this.blockSize = blockSize;
	}

	public void setTimeToLive(int timeToLive)
	{
		this.timeToLive = timeToLive;
	}

	public void setBloomFilter(String bloomFilter)
	{
		this.bloomFilter = bloomFilter;

	}

	public void setScope(int scope)
	{
		this.scope = scope;
	}

	public void setCacheBloomsOnWrite(boolean cache)
	{
		cacheBloomsOnWrite = cache;
	}

	public void setCacheDataOnWrite(boolean cache)
	{
		cacheDataOnWrite = cache;
	}

	public void setCacheIndexesOnWrite(boolean cache)
	{
		cacheIndexesOnWrite = cache;
	}

	public void setCompactionCompression(String compression)
	{
		compactionCompression = Compression.Algorithm.valueOf(compression.toUpperCase());
	}

	public void setCompressTags(boolean compressTags)
	{
		this.compressTags = compressTags;
	}

	public void setEncryptionType(String algorithm)
	{
		encryptionType = algorithm;
	}

	public void setEncryptionKey(String key)
	{
		encryptionKey = key;
	}

	public void setEvictBlocksOnClose(boolean evict)
	{
		evictBlocksOnClose = evict;
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

	public void addConfiguredRemoveConfiguration(KeyValuePair config)
	{
		if (config == null)
		{
			throw new IllegalArgumentException("Configuration can't be null");
		}

		config.validate();
		configsToRemove.add(config);
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

	public void addConfiguredRemoveMetadata(KeyValuePair value)
	{
		if (value == null)
		{
			throw new IllegalArgumentException("Metadata can't be null");
		}

		value.validate();
		metadataToRemove.add(value);
	}

	public HColumnDescriptor createColumnDescriptor()
	{
		HColumnDescriptor column = new HColumnDescriptor(Bytes.toBytes(name));
		updateColumnDescriptor(column);
		return column;
	}

	public HColumnDescriptor getUpdatedColumnDescriptor(HTableDescriptor table)
	{
		HColumnDescriptor column;

		try
		{
			column = table.getFamily(Bytes.toBytes(name));
			if (column == null)
			{
				throw new BuildException("There is no " + name + " column family descriptor in table " + table.getName());
			}

			updateColumnDescriptor(column);

			return column;
		}
		catch (Throwable e)
		{
			throw new BuildException("Failed to get " + name + " column family descriptor from table " + table.getName());
		}
	}

	private void updateColumnDescriptor(HColumnDescriptor column)
	{
		if (maxVersions != null)
		{
			column.setMaxVersions(maxVersions);
		}

		if (minVersions != null)
		{
			column.setMinVersions(minVersions);
		}

		if (keepDeletedCells != null)
		{
			column.setKeepDeletedCells(keepDeletedCells);
		}

		if (compression != null)
		{
			column.setCompressionType(compression);
		}

		if (encodeOnDisk != null)
		{
			column.setEncodeOnDisk(encodeOnDisk);
		}

		if (dataBlockEncoding != null)
		{
			column.setDataBlockEncoding(dataBlockEncoding);
		}

		if (inMemory != null)
		{
			column.setInMemory(inMemory);
		}

		if (blockCache != null)
		{
			column.setBlockCacheEnabled(blockCache);
		}

		if (blockSize != null)
		{
			column.setBlocksize(blockSize);
		}

		if (timeToLive != null)
		{
			column.setTimeToLive(timeToLive);
		}

		if (bloomFilter != null)
		{
			column.setBloomFilterType(BloomType.valueOf(bloomFilter));
		}

		if (scope != null)
		{
			column.setScope(scope);
		}

		if (cacheBloomsOnWrite != null)
		{
			column.setCacheBloomsOnWrite(cacheBloomsOnWrite);
		}

		if (cacheDataOnWrite != null)
		{
			column.setCacheDataOnWrite(cacheDataOnWrite);
		}

		if (cacheIndexesOnWrite != null)
		{
			column.setCacheIndexesOnWrite(cacheIndexesOnWrite);
		}

		if (compactionCompression != null)
		{
			column.setCompactionCompressionType(compactionCompression);
		}

		if (compressTags != null)
		{
			column.setCompressTags(compressTags);
		}

		if (encryptionType != null)
		{
			column.setEncryptionType(encryptionType);
		}

		if (encryptionKey != null)
		{
			column.setEncryptionKey(Bytes.toBytes(encryptionKey));
		}

		if (evictBlocksOnClose != null)
		{
			column.setEvictBlocksOnClose(evictBlocksOnClose);
		}

		if (!configsToRemove.isEmpty())
		{
			for (KeyValuePair pair : configsToRemove)
			{
				column.removeConfiguration(pair.getKey());
			}
		}

		if (!metadataToRemove.isEmpty())
		{
			for (KeyValuePair pair : metadataToRemove)
			{
				column.remove(Bytes.toBytes(pair.getKey()));
			}
		}

		if (!configs.isEmpty())
		{
			for (KeyValuePair pair : configs)
			{
				column.setConfiguration(pair.getKey(), pair.getValue());
			}
		}

		if (!metadata.isEmpty())
		{
			for (KeyValuePair pair : metadata)
			{
				column.setValue(pair.getKey(), pair.getValue());
			}
		}
	}
}
