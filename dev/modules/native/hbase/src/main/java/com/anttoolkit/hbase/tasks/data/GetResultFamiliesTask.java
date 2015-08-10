package com.anttoolkit.hbase.tasks.data;

import java.util.*;

import org.apache.tools.ant.*;
import org.apache.hadoop.hbase.client.*;
import org.apache.hadoop.hbase.util.*;

import com.anttoolkit.hbase.tasks.*;
import com.anttoolkit.general.tasks.collections.array.util.*;

public class GetResultFamiliesTask extends GenericHBaseTask
{
	private String array;
	private String ref;

	public void setArray(String array)
	{
		this.array = array;
	}

	public void setRefid(String reference)
	{
		this.ref = reference;
	}

	@Override
	protected void doHadoopWork() throws BuildException
	{
		Result result = (Result)this.getReference(ref);

		Set<byte[]> families = result.getMap().keySet();
		for (byte[] family : families)
		{
			ArrayManager.add(array, Bytes.toString(family));
		}
	}

	@Override
	protected void hadoopValidate()
	{
		if (array == null)
		{
			throw new BuildException("Array to store column families should be specified");
		}

		if (!ArrayManager.exists(array))
		{
			throw new BuildException("Specified array '" + array + "' doesn't exist");
		}

		if (ref == null)
		{
			throw new BuildException("Reference to result of a GET operation wasn't specified");
		}

		Object obj = this.getReference(ref);
		if (obj == null || !(obj instanceof Result))
		{
			throw new BuildException("Reference '" + ref + "' to a RESULT object, contains incorrect object");
		}
	}
}
