package com.anttoolkit.hbase.tasks.data.util.scan.comparator;

import org.apache.hadoop.hbase.filter.*;
import org.apache.tools.ant.*;

import com.anttoolkit.general.tasks.*;

public class GenericComparatorConfig extends ComparatorConfig
{
	private String ref;

	public void setValue(String value)
	{
		throw new BuildException("Setting value not supported by GenericComparator config");
	}

	public void addText(String value)
	{
		throw new BuildException("Setting value not supported by GenericComparator config");
	}

	public void setRefid(String ref)
	{
		if (ref == null)
		{
			throw new IllegalArgumentException("Can't set null reference for GenericComparator config");
		}

		this.ref = ref;
	}

	@Override
	public ByteArrayComparable getComparator(GenericTask task)
	{
		validate(task);
		return (ByteArrayComparable)task.getReference(ref);
	}

	@Override
	public void validate(GenericTask task)
	{
		if (ref == null || !(task.getReference(ref) instanceof ByteArrayComparable))
		{
			throw new IllegalArgumentException("Reference to ByteArrayComparable object wasn't specified for GenericComparator config");
		}
	}
}
