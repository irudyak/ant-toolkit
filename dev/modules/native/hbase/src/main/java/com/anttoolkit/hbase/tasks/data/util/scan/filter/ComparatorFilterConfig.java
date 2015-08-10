package com.anttoolkit.hbase.tasks.data.util.scan.filter;

import java.util.*;

import org.apache.hadoop.hbase.filter.*;
import org.apache.tools.ant.*;

import com.anttoolkit.general.tasks.*;
import com.anttoolkit.hbase.tasks.data.util.scan.comparator.*;

public abstract class ComparatorFilterConfig implements IFilterConfig
{
	private static final Map<String, CompareFilter.CompareOp> OPERATIONS;

	static
	{
		Map<String, CompareFilter.CompareOp> map = new HashMap<String, CompareFilter.CompareOp>();
		map.put("LESS", CompareFilter.CompareOp.LESS);
		map.put("LESS_OR_EQUAL", CompareFilter.CompareOp.LESS_OR_EQUAL);
		map.put("EQUAL", CompareFilter.CompareOp.EQUAL);
		map.put("NOT_EQUAL", CompareFilter.CompareOp.NOT_EQUAL);
		map.put("GREATER_OR_EQUAL", CompareFilter.CompareOp.GREATER_OR_EQUAL);
		map.put("GREATER", CompareFilter.CompareOp.GREATER);
		map.put("NO_OP", CompareFilter.CompareOp.NO_OP);
		OPERATIONS = map;
	}

	private ComparatorConfig comparatorConfig;
	private CompareFilter.CompareOp operation;

	public void setOperation(String operation)
	{
		this.operation = OPERATIONS.get(operation);
		if (this.operation == null)
		{
			throw new BuildException("Incorrect compare operation specified for RowFilter: " + operation);
		}
	}

	public void addConfiguredComparator(GenericComparatorConfig config)
	{
		if (comparatorConfig != null)
		{
			throw new BuildException("It is not possible to add multiple comparators to a Filter");
		}

		comparatorConfig = config;
	}

	public void addConfiguredBinaryComparator(BinaryComparatorConfig config)
	{
		if (comparatorConfig != null)
		{
			throw new BuildException("It is not possible to add multiple comparators to a Filter");
		}

		comparatorConfig = config;
	}

	public void addConfiguredBinaryPrefixComparator(BinaryPrefixComparatorConfig config)
	{
		if (comparatorConfig != null)
		{
			throw new BuildException("It is not possible to add multiple comparators to a Filter");
		}

		comparatorConfig = config;
	}

	public void addConfiguredBitComparatorConfig(BitComparatorConfig config)
	{
		if (comparatorConfig != null)
		{
			throw new BuildException("It is not possible to add multiple comparators to a Filter");
		}

		comparatorConfig = config;
	}

	public void addConfiguredNullComparator(NullComparatorConfig config)
	{
		if (comparatorConfig != null)
		{
			throw new BuildException("It is not possible to add multiple comparators to a Filter");
		}

		comparatorConfig = config;
	}

	public void addConfiguredRegexStringComparator(RegexStringComparatorConfig config)
	{
		if (comparatorConfig != null)
		{
			throw new BuildException("It is not possible to add multiple comparators to a Filter");
		}

		comparatorConfig = config;
	}

	public void addConfiguredSubstringComparator(SubstringComparatorConfig config)
	{
		if (comparatorConfig != null)
		{
			throw new BuildException("It is not possible to add multiple comparators to a Filter");
		}

		comparatorConfig = config;
	}

	@Override
	public void validate(GenericTask task)
	{
		if (comparatorConfig == null)
		{
			throw new BuildException("There is no comparator specified for a Filter");
		}

		if (operation == null)
		{
			throw new BuildException("There is no compare operation specified for a Filter");
		}
	}

	protected ByteArrayComparable getComparator(GenericTask task)
	{
		return comparatorConfig.getComparator(task);
	}

	protected CompareFilter.CompareOp getOperation()
	{
		return operation;
	}
}
