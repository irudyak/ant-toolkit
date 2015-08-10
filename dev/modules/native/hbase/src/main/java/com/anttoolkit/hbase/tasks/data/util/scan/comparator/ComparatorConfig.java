package com.anttoolkit.hbase.tasks.data.util.scan.comparator;

import org.apache.hadoop.hbase.filter.*;
import org.apache.tools.ant.*;

import com.anttoolkit.general.tasks.*;
import com.anttoolkit.hbase.tasks.data.util.*;

public abstract class ComparatorConfig
{
	private String value;
	private String reference;
	private CellValueType type = CellValueType.STRING;

	public abstract ByteArrayComparable getComparator(GenericTask task);

	public void validate(GenericTask task)
	{
		if ((reference == null && value == null) ||
			(reference != null && value != null))
		{
			throw new BuildException("Incorrect comparator configuration, either value or reference should be specified");
		}

		if (reference != null && (task.getReference(reference) == null ||
			!(task.getReference(reference) instanceof byte[])))
		{
			throw new BuildException("Incorrect comparator configuration, invalid reference '" +
					reference + "' was specified");
		}
	}

	public void setValue(String value)
	{
		this.value = value;
	}

	public void addText(String value)
	{
		setValue(value);
	}

	public void setReference(String reference)
	{
		this.reference = reference;
	}

	protected byte[] getValue(GenericTask task)
	{
		return value != null ? type.encode(value) : CellValueType.encodeObject(task.getReference(reference));
	}
}
