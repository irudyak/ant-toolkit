package com.anttoolkit.hbase.tasks.data;

import org.apache.hadoop.hbase.client.*;
import org.apache.tools.ant.*;

import com.anttoolkit.hbase.tasks.*;
import com.anttoolkit.hbase.tasks.data.util.*;

public class GetResultRowkeyTask extends GenericHBaseTask
{
	private String property;
	private String resultRef;
	private String reference;
	private String type;

	public void setProperty(String property)
	{
		this.property = property;
	}

	public void setReference(String reference)
	{
		this.reference = reference;
	}

	public void setType(String type)
	{
		if (type == null)
		{
			throw new IllegalArgumentException("Cell value type can't be null");
		}

		this.type = type.trim().toUpperCase();
	}

	public void setResultRef(String resultRef)
	{
		this.resultRef = resultRef;
	}

	@Override
	protected void doHadoopWork() throws BuildException
	{
		Result result = (Result)this.getReference(resultRef);

		if (property != null)
		{
			setPropertyThreadSafe(property, getType().decode(result.getRow()).toString());
		}

		if (reference != null)
		{
			this.setReference(reference, getType().decode(result.getRow()));
		}
	}

	@Override
	protected void hadoopValidate()
	{
		if (property == null && reference == null)
		{
			throw new BuildException("Either property or reference to store rowkey should be specified");
		}

		if (resultRef == null)
		{
			throw new BuildException("Reference to a RESULT object wasn't specified");
		}

		Object obj = this.getReference(resultRef);
		if (obj == null || !(obj instanceof Result))
		{
			throw new BuildException("Reference '" + resultRef + "' to a RESULT object, contains incorrect object");
		}
	}

	private CellValueType getType()
			throws IllegalArgumentException
	{
		if (type == null)
		{
			type = reference != null ? CellValueType.BYTES.toString() : CellValueType.STRING.toString();
		}

		return CellValueType.valueOf(type);
	}
}
