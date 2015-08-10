package com.anttoolkit.hbase.tasks.data.util.scan.comparator;

import org.apache.hadoop.hbase.filter.*;
import org.apache.tools.ant.BuildException;

import com.anttoolkit.general.tasks.*;

public class BitComparatorConfig extends ComparatorConfig
{
	private BitComparator.BitwiseOp operator;

	public void setOperator(String operator)
	{
		try
		{
			this.operator = BitComparator.BitwiseOp.valueOf(operator.toUpperCase());
		}
		catch (IllegalArgumentException e)
		{
			throw new BuildException("Incorrect operator '" + operator + "' specified for BitComparator");
		}
	}

	@Override
	public ByteArrayComparable getComparator(GenericTask task)
	{
		validate(task);
		return new BitComparator(getValue(task), operator);
	}

	@Override
	public void validate(GenericTask task)
	{
		super.validate(task);

		if (operator == null)
		{
			throw new BuildException("Operator should be specified for BitComparator");
		}
	}
}
