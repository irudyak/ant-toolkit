package com.anttoolkit.hbase.tasks.data.util.scan.filter;

import java.util.*;

import org.apache.hadoop.hbase.filter.*;
import org.apache.tools.ant.*;

import com.anttoolkit.general.tasks.*;

public abstract class FiltersCollectionConfig implements IFilterConfig
{
	private List<IFilterConfig> filters = new LinkedList<IFilterConfig>();

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

	@Override
	public void validate(GenericTask task)
	{
		if (filters.isEmpty())
		{
			throw new BuildException("No filters specified");
		}
	}

	protected Filter getSummaryFilter(GenericTask task)
	{
		validate(task);

		if (filters.size() == 1)
		{
			return filters.get(0).getFilter(task);
		}

		FilterList list = new FilterList();

		for (IFilterConfig config : filters)
		{
			list.addFilter(config.getFilter(task));
		}

		return list;
	}
}
