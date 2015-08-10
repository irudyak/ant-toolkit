package com.anttoolkit.hbase.tasks.data.util;

import java.io.*;
import java.util.*;

import org.apache.hadoop.hbase.client.*;
import org.apache.hadoop.hbase.util.Bytes;
import org.apache.tools.ant.*;

import com.anttoolkit.hbase.tasks.*;
import com.anttoolkit.general.refs.*;

public class GetAction extends GenericAction<Get>
{
	private Boolean cacheBlocks;
	private Integer maxVersions;
	private Long minStamp;
	private Long maxStamp;
	private Long timestamp;
	private String reference;
	private List<CellToGet> cells = new LinkedList<CellToGet>();

	public GetAction(GenericHBaseTask task)
	{
		super(task);
	}

	public void setReference(String ref)
	{
		reference = ref;
	}

	public void setCacheBlocks(boolean cache)
	{
		this.cacheBlocks = cache;
	}

	public void setMaxVersions(int maxVersions)
	{
		this.maxVersions = maxVersions;
	}

	public void setMinTimestamp(long timestamp)
	{
		this.minStamp = timestamp;
	}

	public void setMaxTimestamp(long timestamp)
	{
		this.maxStamp = timestamp;
	}

	public void setTimestamp(long timestamp)
	{
		this.timestamp = timestamp;
	}

	public void addConfiguredCell(CellToGet cell)
	{
		if (cell == null)
		{
			throw new IllegalArgumentException("Can't add null cell to a GET operation");
		}

		cells.add(cell);
	}

	@Override
	public Get getAction()
	{
		validate();

		Get get = new Get(getRowkey());

		if (cacheBlocks != null)
		{
			get.setCacheBlocks(cacheBlocks);
		}

		if (maxVersions != null)
		{
			try
			{
				get.setMaxVersions(maxVersions);
			}
			catch (IOException e)
			{
				throw new BuildException("Invalid maxVersions specified for GET operation: " + toString());
			}
		}

		if (minStamp != null && maxStamp != null)
		{
			try
			{
				get.setTimeRange(minStamp, maxStamp);
			}
			catch (IOException e)
			{
				throw new BuildException("Invalid timeRange specified for GET operation: " + toString());
			}
		}

		if (timestamp != null)
		{
			try
			{
				get.setTimeStamp(timestamp);
			}
			catch (Throwable e)
			{
				throw new BuildException("Failed to set timestamp: " + timestamp);
			}
		}

		for (CellToGet cell : cells)
		{
			cell.addToGet(get);
		}

		return get;
	}

	@Override
	public void execute(HTableInterface table)
	{
		validate();

		try
		{
			processExecutionResult(table.get(getAction()), table.getName().getNameAsString());
		}
		catch (IOException e)
		{
			throw new BuildException("Failed to execute GET against " + table.getName().getNameAsString() + " table: " + toString(), e);
		}
	}

	@Override
	public void processExecutionResult(Result result, String table)
	{
		if (reference != null)
		{
			task.setReference(reference, result);
		}

		for (CellToGet cell : cells)
		{
			if ((cell.getProperty() == null || cell.getProperty().trim().isEmpty()) &&
				(cell.getFile() == null || cell.getFile().trim().isEmpty()))
			{
				continue;
			}

			if (cell.processResult(result, task))
			{
				continue;
			}

			if (cell.getVersion() != null)
			{
				throw new BuildException("There are no column " + cell.getColumnFamily() + ":" + cell.getColumn() +
						" with version number " + cell.getVersion() +
						" in the GET operation result for the table " + table + ": " + toString());
			}

			if (cell.getTimestamp() != null)
			{
				throw new BuildException("There are no column " + cell.getColumnFamily() + ":" + cell.getColumn() +
						" with timestamp " + cell.getTimestamp() +
						" in the GET operation result for the table " + table + ": " + toString());
			}

			throw new BuildException("There are no column " + cell.getColumnFamily() + ":" + cell.getColumn() +
					" in the GET operation result for the table " + table + ": " + toString());
		}
	}

	@Override
	public void validate()
	{
		super.validate();

		if (maxVersions != null && maxVersions <= 0)
		{
			throw new BuildException("Incorrect maxVersions specified for a GET operation: " + toString());
		}

		if ((minStamp != null && maxStamp == null) ||
			(minStamp == null && maxStamp != null) ||
			(minStamp != null && maxStamp != null && minStamp > maxStamp))
		{
			throw new BuildException("Incorrect time range specified for a GET operation: " + toString());
		}

		if (timestamp != null && timestamp <= 0)
		{
			throw new BuildException("Incorrect timestamp specified for a GET operation: " + toString());
		}

		if (timestamp != null && (minStamp != null || maxStamp != null))
		{
			throw new BuildException("Either timestamp or time range should be specified for a GET operation: " + toString());
		}

		if (!checkCellsValid())
		{
			throw new BuildException("Incorrect cells specification for a GET operation: " + toString());
		}
	}

	@Override
	public String toString()
	{
		StringBuilder builder = new StringBuilder();
		builder.append("GET{rowkey=").append(getRowkeyAsString() == null ? "null" : getRowkeyAsString());

		if (cacheBlocks != null)
		{
			builder.append(", cacheBlocks=").append(cacheBlocks.toString());
		}

		if (maxVersions != null)
		{
			builder.append(", maxVersions=").append(maxVersions.toString());
		}

		if (minStamp != null || maxStamp != null)
		{
			builder.append(", timeRange=(").append(minStamp == null ? "null" : minStamp.toString());
			builder.append("-").append(maxStamp == null ? "null" : maxStamp.toString()).append(")");
		}

		if (timestamp != null)
		{
			builder.append(", timestamp=").append(timestamp.toString());
		}

		for (CellToGet cell : cells)
		{
			builder.append(", [").append(cell.toString()).append("]");
		}

		builder.append("}");

		return builder.toString();
	}

	private boolean checkCellsValid()
	{
		for (CellToGet cell : cells)
		{
			if (!cell.isValid())
			{
				return false;
			}
		}

		return true;
	}
}
