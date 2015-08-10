package com.anttoolkit.hbase.tasks.data.util.scan;

import java.io.*;
import java.util.*;

import org.apache.hadoop.hbase.client.*;
import org.apache.hadoop.hbase.filter.*;
import org.apache.hadoop.hbase.util.*;
import org.apache.tools.ant.*;

import com.anttoolkit.hbase.tasks.data.util.scan.filter.*;
import com.anttoolkit.hbase.tasks.data.util.*;
import com.anttoolkit.hbase.tasks.*;
import com.anttoolkit.general.tasks.*;

public class ScanAction
{
	private Integer batch;
	private Integer caching;
	private Boolean loadOnDemand;
	private Long maxResults;
	private Integer maxResultsPerFamily;
	private Integer maxVersions;
	private Long minStamp;
	private Long maxStamp;
	private Long timestamp;
	private String startRow;
	private String stopRow;
	private List<CellToGet> cells = new LinkedList<CellToGet>();
	private List<IFilterConfig> filters = new LinkedList<IFilterConfig>();
	private Scan scan;

	public void setBatch(int batch)
	{
		this.batch = batch;
	}

	public void setCaching(int caching)
	{
		this.caching = caching;
	}

	public void setLoadOnDemand(boolean loadOnDemand)
	{
		this.loadOnDemand = loadOnDemand;
	}

	public void setMaxResults(long maxResults)
	{
		this.maxResults = maxResults;
	}

	public void setMaxResultsPerFamily(int maxResultsPerFamily)
	{
		this.maxResultsPerFamily = maxResultsPerFamily;
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

	public void setStartRow(String startRow)
	{
		this.startRow = startRow;
	}

	public void setStopRow(String stopRow)
	{
		this.stopRow = stopRow;
	}

	public void addConfiguredCell(CellToGet cell)
	{
		if (cell == null)
		{
			throw new IllegalArgumentException("Can't add null cell to a SCAN operation");
		}

		cells.add(cell);
	}

	public void addConfiguredColumnCountGetFilter(ColumnCountGetFilterConfig config)
	{
		filters.add(config);
	}

	public void addConfiguredColumnPaginationFilter(ColumnPaginationFilterConfig config)
	{
		filters.add(config);
	}

	public void addConfiguredColumnPrefixFilter(ColumnPrefixFilterConfig config)
	{
		filters.add(config);
	}

	public void addConfiguredDependentColumnFilter(DependentColumnFilterConfig config)
	{
		filters.add(config);
	}

	public void addConfiguredFamilyFilter(FamilyFilterConfig config)
	{
		filters.add(config);
	}

	public void addConfiguredFirstKeyOnlyFilter(FirstKeyOnlyFilterConfig config)
	{
		filters.add(config);
	}

	public void addConfiguredGenericFilter(GenericFilterConfig config)
	{
		filters.add(config);
	}

	public void addConfiguredInclusiveStopFilter(InclusiveStopFilterConfig config)
	{
		filters.add(config);
	}

	public void addConfiguredKeyOnlyFilter(KeyOnlyFilterConfig config)
	{
		filters.add(config);
	}

	public void addConfiguredPageFilter(PageFilterConfig config)
	{
		filters.add(config);
	}

	public void addConfiguredPrefixFilter(PrefixFilterConfig config)
	{
		filters.add(config);
	}

	public void addConfiguredQualifierFilter(QualifierFilterConfig config)
	{
		filters.add(config);
	}

	public void addConfiguredRandomRowFilter(RandomRowFilterConfig config)
	{
		filters.add(config);
	}

	public void addConfiguredRowFilter(RowFilterConfig config)
	{
		filters.add(config);
	}

	public void addConfiguredSingleColumnValueExcludeFilter(SingleColumnValueExcludeFilterConfig config)
	{
		filters.add(config);
	}

	public void addConfiguredSingleColumnValueFilter(SingleColumnValueFilterConfig config)
	{
		filters.add(config);
	}

	public void addConfiguredTimestampsFilter(TimestampsFilterConfig config)
	{
		filters.add(config);
	}

	public void addConfiguredValueFilter(ValueFilterConfig config)
	{
		filters.add(config);
	}

	public void addConfiguredSkipFilter(SkipFilterConfig config)
	{
		filters.add(config);
	}

	public void addConfiguredWhileMatchFilter(WhileMatchFilterConfig config)
	{
		filters.add(config);
	}

	public ResultScanner getScanner(HTableInterface table, GenericTask task)
	{
		try
		{
			return table.getScanner(getScanObject(task));
		}
		catch (IOException e)
		{
			throw new BuildException("Failed to execute SCAN against " + table.getName().getNameAsString() + " table: " + toString(), e);
		}
	}

	public void processResult(Result result, String table, GenericHBaseTask task)
	{
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
						" in the SCAN operation result for the table " + table + ": " + toString());
			}

			if (cell.getTimestamp() != null)
			{
				throw new BuildException("There are no column " + cell.getColumnFamily() + ":" + cell.getColumn() +
						" with timestamp " + cell.getTimestamp() +
						" in the SCAN operation result for the table " + table + ": " + toString());
			}

			throw new BuildException("There are no column " + cell.getColumnFamily() + ":" + cell.getColumn() +
					" in the SCAN operation result for the table " + table + ": " + toString());
		}
	}

	public void validate(GenericTask task)
	{
		if (stopRow != null && startRow == null)
		{
			throw new BuildException("Start row should be specified for a SCAN operation: " + toString());
		}

		if (maxVersions != null && maxVersions <= 0)
		{
			throw new BuildException("Incorrect maxVersions specified for a SCAN operation: " + toString());
		}

		if ((minStamp != null && maxStamp == null) ||
			(minStamp == null && maxStamp != null) ||
			(minStamp != null && maxStamp != null && minStamp > maxStamp))
		{
			throw new BuildException("Incorrect time range specified for a SCAN operation: " + toString());
		}

		if (timestamp != null && timestamp <= 0)
		{
			throw new BuildException("Incorrect timestamp specified for a SCAN operation: " + toString());
		}

		if (timestamp != null && (minStamp != null || maxStamp != null))
		{
			throw new BuildException("Either timestamp or time range should be specified for a SCAN operation: " + toString());
		}

		if (batch != null && batch <= 0)
		{
			throw new BuildException("Incorrect batch specified for a SCAN operation: " + toString());
		}

		if (caching != null && caching <= 0)
		{
			throw new BuildException("Incorrect caching specified for a SCAN operation: " + toString());
		}

		if (maxResults != null && maxResults <= 0)
		{
			throw new BuildException("Incorrect maxResults specified for a SCAN operation: " + toString());
		}

		if (maxResultsPerFamily != null && maxResultsPerFamily <= 0)
		{
			throw new BuildException("Incorrect maxResultsPerFamily specified for a SCAN operation: " + toString());
		}

		if (!checkCellsValid())
		{
			throw new BuildException("Incorrect cells specification for a SCAN operation: " + toString());
		}

		try
		{
			validateFilters(task);
		}
		catch (Throwable e)
		{
			throw new BuildException("Incorrect filters specification for a SCAN operation: " + toString(), e);
		}
	}

	@Override
	public String toString()
	{
		StringBuilder builder = new StringBuilder();

		if (startRow != null)
		{
			builder.append(", startRow=").append(startRow);
		}

		if (stopRow != null)
		{
			builder.append(", stopRow=").append(stopRow);
		}

		if (batch != null)
		{
			builder.append(", batch=").append(batch.toString());
		}

		if (caching != null)
		{
			builder.append(", caching=").append(caching.toString());
		}

		if (loadOnDemand != null)
		{
			builder.append(", loadOnDemand=").append(loadOnDemand.toString());
		}

		if (maxResults != null)
		{
			builder.append(", maxResults=").append(maxResults.toString());
		}

		if (maxResultsPerFamily != null)
		{
			builder.append(", maxResultsPerFamily=").append(maxResultsPerFamily.toString());
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

		if (!cells.isEmpty())
		{
			for (CellToGet cell : cells)
			{
				builder.append(", [").append(cell.toString()).append("]");
			}
		}

		if (!filters.isEmpty())
		{
			int i = 0;
			builder.append(", filters: ");

			for (IFilterConfig config : filters)
			{
				if (i != 0)
				{
					builder.append(", ");
				}

				builder.append("[").append(config.toString()).append("]");
				i++;
			}
		}

		return "SCAN{" + builder.substring(1) + "}";
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

	private void validateFilters(GenericTask task)
	{
		for (IFilterConfig config : filters)
		{
			config.validate(task);
		}
	}

	private Scan getScanObject(GenericTask task)
	{
		if (scan != null)
		{
			return scan;
		}

		validate(task);

		scan = startRow != null && stopRow != null ?
				new Scan(Bytes.toBytes(startRow), Bytes.toBytes(stopRow)) :
				startRow != null ?
				new Scan(Bytes.toBytes(startRow)) :
				new Scan();

		if (batch != null)
		{
			scan.setBatch(batch);
		}

		if (caching != null)
		{
			scan.setCaching(caching);
		}

		if (loadOnDemand != null)
		{
			scan.setLoadColumnFamiliesOnDemand(loadOnDemand);
		}

		if (maxResults != null)
		{
			scan.setMaxResultSize(maxResults);
		}

		if (maxResultsPerFamily != null)
		{
			scan.setMaxResultsPerColumnFamily(maxResultsPerFamily);
		}

		if (maxVersions != null)
		{
			scan.setMaxVersions(maxVersions);
		}

		if (minStamp != null && maxStamp != null)
		{
			try
			{
				scan.setTimeRange(minStamp, maxStamp);
			}
			catch (IOException e)
			{
				throw new BuildException("Invalid timeRange specified for SCAN operation: " + toString());
			}
		}

		if (timestamp != null)
		{
			try
			{
				scan.setTimeStamp(timestamp);
			}
			catch (Throwable e)
			{
				throw new BuildException("Invalid timestamp specified for SCAN operation: " + timestamp);
			}
		}

		for (CellToGet cell : cells)
		{
			if (cell.getColumn() != null)
			{
				scan.addColumn(Bytes.toBytes(cell.getColumnFamily()), Bytes.toBytes(cell.getColumn()));
			}
			else
			{
				scan.addFamily(Bytes.toBytes(cell.getColumnFamily()));
			}
		}

		Filter filter = getFilter(task);
		if (filter != null)
		{
			scan.setFilter(filter);
		}

		return scan;
	}

	public Filter getFilter(GenericTask task)
	{
		if (filters.isEmpty())
		{
			return null;
		}

		if (filters.size() == 1)
		{
			return filters.get(0).getFilter(task);
		}

		FilterList filterList = new FilterList();

		for (IFilterConfig config : filters)
		{
			filterList.addFilter(config.getFilter(task));
		}

		return filterList;
	}
}
